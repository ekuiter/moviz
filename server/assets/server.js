function Server(initialized) {
    var self = this;
    if (!(this instanceof Server))
	return new Server();
    
    this.invalidatables = [];
    this.destructiveInvalidatables = [];
    this.cache = {};
    this.setup = this.callFn("setup");
    this.getNodes = this.getFn("graph/nodes", Movie, "nodes");
    this.getEdges = this.getFn("graph/edges", RoleEdge, "edges");
    this.tmdbSearch = this.cachedFn(
	this.thenFn(this.callFn("tmdb/search"), asObject), "tmdbSearch");
    this.clear = this.destructiveFn(this.callFn("clear"));
    this.add = this.destructiveFn(this.callFn("add"));
    this.progress = this.callFn("progress");
    this.abort = this.callFn("abort");
    this.update = this.invalidateFn(this.callFn("update", ["unlabeled", "detailed", "condensed",
							   "weighted", "top-actors"]));
    this.filterNodes = this.invalidateFn(
	this.callFilterFn("filter/node", ["movie-filter", "neighborhood-filter"]));
    this.filterEdges = this.invalidateFn(
	this.callFilterFn("filter/edge", ["gender-filter", "same-character-filter",
					  "billing-filter", "actor-filter", "type-filter"]));
    this.search = this.cachedFn(this.thenFn(this.callFn("search"), asList), "search");
    this.inverseSearch = this.cachedFn(
	this.thenFn(this.callFn("inverse-search"), asList), "inverseSearch");
    this.details = this.cachedFn(this.callFn("details"), "details");
    this.suggest = this.thenFn(this.callFn("suggest"), asList);
    this.eval = this.callFn("eval");
    this.loadGraph = this.destructiveFn(this.callFn("graph/load"));

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

    destructiveFn: function(fn) {
	var self = this;
	return function() {
	    return self.invalidateFn(fn).apply(this, arguments).then(function() {
		self.destructiveInvalidatables.forEach(function(obj) {
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

    shouldInvalidateOnDestruction: function(obj) {
	this.destructiveInvalidatables.push(obj);
    },

    saveGraph: function() {
	document.location.href = "/graph/save/";
    },

    exportGraph: function() {
	document.location.href = "/graph/export/";
    },

    getFn: function(fn, klass, cacheProp) {
	var self = this;
	return this.thenFn(this.callFn(fn), function(data) {
	    return self.cache[cacheProp] = asList(data).map(klass);
	});
    },

    cachedFn: function(fn, cacheProp) {
	var self = this;
	self.cache[cacheProp] = {};
	return function() {
	    var key = JSON.stringify(Array.prototype.slice.call(arguments));
	    if (self.cache[cacheProp][key])
		return instantPromise(self.cache[cacheProp][key]);
	    else
		return fn.apply(this, arguments).then(function(data) {
		    return self.cache[cacheProp][key] = data;
		});
	};
    }
};
