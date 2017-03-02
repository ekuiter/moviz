var server = { debug: false };

function assert(condition, message) {
    if (!condition)
        throw message || "Assertion failed";
}

function assertLegalValues(values, legalValues) {
    if (legalValues)
	values.forEach(function(value) {
	    assert(legalValues.indexOf(value) !== -1, value + " is not a legal value");
	});
}

function assertLegalForm(obj, legalValues, level) {
    level = level || 0;
    if (Array.isArray(obj)) {
	assert(obj.length > 0, "empty form encountered");
	assert(typeof obj[0] === "string", obj[0] + " does not denote a function");
	assertLegalValues([obj[0]], legalValues);
	obj.slice(1).forEach(function(obj) {
	    assertLegalForm(obj, legalValues, level + 1);
	});
    } else if (level == 0)
	throw "top level must be a form";
    else if (Number.isInteger(obj) || typeof obj === "string");
    else
	throw obj + " does not denote an argument";
}

server.callFn = function(fn, legalValues) {
    return function(arr) {
	var args = Array.isArray(arr) ? arr : Array.prototype.slice.call(arguments);
	assertLegalValues(args, legalValues);
	var url = "/" + fn + "/" + args.join("/");
	if (server.debug)
	    console.log(url);
	else
	    return $.ajax(url);
    };
};

server.callMultiFn = function(fn, legalValues) {
    return function() {
	var args = Array.prototype.slice.call(arguments);
	return server.callFn(fn, legalValues).apply(this, args);
    };
};

server.callFilterFn = function(type, legalValues) {
    return function(filter) {
	legalValues = legalValues || [];
	legalValues.push("or-filter", "and-filter", "not-filter", "all-filter");
	assertLegalForm(filter, legalValues);
	return server.callFn("filter/" + type)(JSON.stringify(filter));
    };
}

server.clear = server.callFn("clear");
server.add = server.callMultiFn("add");
server.update = server.callMultiFn("update", ["unlabeled", "detailed", "condensed",
					      "weighted", "top-actors"]);
server.filterNodes = server.callFilterFn("node", ["movie-filter"]);
server.filterEdges = server.callFilterFn("edge", ["gender-filter", "same-character-filter",
						  "billing-filter"]);
server.getNodes = server.callFn("graph/nodes");

$(function() {
    
});
