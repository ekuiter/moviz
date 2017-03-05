function Server(debug) {
    if (!(this instanceof Server))
	return new Server(debug);
    
    this.debug = debug;
    this.invalidatables = [];
    this.getNodes = this.callFn("graph/nodes");
    this.getEdges = this.callFn("graph/edges");
    this.clear = this.invalidateFn(this.callFn("clear"));
    this.add = this.invalidateFn(this.callFn("add"));
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
    this.update();
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
    }
};
