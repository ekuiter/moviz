function Server(initialized) {
    var self = this;
    if (!(this instanceof Server))
	return new Server(debug);
    
    this.invalidatables = [];
    this.setup = this.callFn("setup");
    this.getNodes = this.thenFn(this.callFn("graph/nodes"), function(data) {
	asList(data).forEach(function(node) {
	    var metadata = false;
	    node.getMetadata = function() {
		if (metadata === false)
		    return self.tmdbSearch("movies", node.title).then(function(data) {
			return metadata = asObject(data);
		    });
		else
		    return instantPromise(metadata);
	    };
	});
	return self.cachedNodes = asList(data);
    });
    this.getEdges = this.callFn("graph/edges");
    this.tmdbSearch = this.callFn("tmdb/search");
    this.clear = this.invalidateFn(this.callFn("clear"));
    this.add = this.invalidateFn(this.callFn("add"));
    this.progress = this.callFn("progress");
    this.abort = this.callFn("abort");
    this.update = this.invalidateFn(this.callFn("update", ["unlabeled", "detailed", "condensed",
							   "weighted", "top-actors"]));
    this.filterNodes = this.invalidateFn(
	this.callFilterFn("filter/node", ["movie-filter", "neighborhood-filter"]));
    this.filterEdges = this.invalidateFn(
	this.callFilterFn("filter/edge", ["gender-filter", "same-character-filter",
					  "billing-filter", "actor-filter"]));
    this.search = this.callFn("search");
    this.inverseSearch = this.callFn("inverse-search");
    this.suggest = this.callFn("suggest");
    this.eval = this.callFn("eval");
    this.loadGraph = this.invalidateFn(this.callFn("graph/load"));

    var setupFinished = false;
    defer(function() {
	if (!setupFinished)
	    $("#setup-dialog").dialog("open");
    }, 500);
    this.setup().always(function() {
	setupFinished = true;
    }).then(function() {
	$("#setup-dialog").dialog("close");
    }).then(function() {
	return self.update();
    }).then(initialized);
};

Server.prototype = {
    callFn: function(fn, legalValues) {
	var self = this;
	return function(arr) {
	    var args = Array.isArray(arr) ? arr : Array.prototype.slice.call(arguments);
	    args = args.filter(function(arg) { return arg !== "." && arg !== ".."; });
	    assertLegalValues(args, legalValues);
	    var url = "/" + fn + "/" + args.join("/");
	    if (self.debug) {
		console.log(url);
		return instantPromise();
	    } else
		return $.ajax(url).fail(function(xhr) {
		    App().reportError(xhr.responseText);
		});
	};
    },

    thenFn: function(fn, cb) {
	return function() {
	    return fn.apply(this, arguments).then(cb);
	};
    },

    call: function(fn, legalValues) {
	return this.callFn(fn, legalValues)(Array.prototype.slice.call(arguments, 2));
    },

    callFilterFn: function(fn, legalValues) {
	var self = this;
	return function(filter) {
	    legalValues = legalValues || [];
	    legalValues.push("or-filter", "and-filter", "not-filter", "all-filter");
	    assertLegalForm(filter, legalValues);
	    return self.callFn(fn)(JSON.stringify(filter));
	};
    },

    invalidateFn: function(fn) {
	var self = this;
	return function() {
	    return fn.apply(this, arguments).then(function() {
		$("#graph").prop("data", "/assets/graph.svg?" + new Date().getTime());
		self.invalidatables.forEach(function(obj) {
		    obj.invalidate();
		});
	    });
	};
    },

    invalidate: function(fn) {
	fn = fn || instantPromise;
	return this.invalidateFn(fn)(Array.prototype.slice.call(arguments, 1));
    },

    shouldInvalidate: function(obj) {
	this.invalidatables.push(obj);
    },

    saveGraph: function() {
	document.location.href = "/graph/save/";
    },

    exportGraph: function() {
	document.location.href = "/graph/export/";
    }
};
