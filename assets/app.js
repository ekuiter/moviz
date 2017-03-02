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

function attachInputEvent(sel, cb) {
    $(sel).keypress(function(e) {
	if (e.which == 13)
            cb($(sel).val());
    });
}

function instantPromise() {
    return $.Deferred().resolve().promise();
}

server.callFn = function(fn, legalValues) {
    return function(arr) {
	var args = Array.isArray(arr) ? arr : Array.prototype.slice.call(arguments);
	assertLegalValues(args, legalValues);
	var url = "/" + fn + "/" + args.join("/");
	if (server.debug) {
	    console.log(url);
	    return instantPromise();
	} else
	    return $.ajax(url).fail(function(res) {
		alert(res.responseText);
	    });
    };
};

server.callFilterFn = function(fn, legalValues) {
    return function(filter) {
	legalValues = legalValues || [];
	legalValues.push("or-filter", "and-filter", "not-filter", "all-filter");
	assertLegalForm(filter, legalValues);
	return server.callFn(fn)(JSON.stringify(filter));
    };
};

server.updatingFn = function(fn) {
    return function() {
	return fn.apply(this, arguments).then(function() {
	    $("#graph").attr("data", "/assets/graph.svg?" + new Date().getTime());
	    $.ajax("/state/").then(function(res) {
		$("#state").html(res);
	    });
	});
    };
};

server.getNodes = server.callFn("graph/nodes");
server.getEdges = server.callFn("graph/edges");
server.clear = server.updatingFn(server.callFn("clear"));
server.add = server.updatingFn(server.callFn("add"));
server.update = server.updatingFn(server.callFn("update", ["unlabeled", "detailed", "condensed",
							   "weighted", "top-actors"]));
server.filterNodes = server.updatingFn(server.callFilterFn("filter/node", ["movie-filter"]));
server.filterEdges = server.updatingFn(
    server.callFilterFn("filter/edge", ["gender-filter", "same-character-filter",
					"billing-filter"]));
server.search = server.callFn("search");
server.inverseSearch = server.callFn("inverse-search");

$(function() {
    window.onerror = alert;
    
    attachInputEvent("#update", function(text) {
	server.update(text.split("/"));
    });

    attachInputEvent("#add", function(text) {
	server.add(text.split("/"));
    });

    attachInputEvent("#filter-nodes", function(text) {
	server.filterNodes(JSON.parse(text));
    });

    attachInputEvent("#filter-edges", function(text) {
	server.filterEdges(JSON.parse(text));
    });

    server.updatingFn(instantPromise)();
});
