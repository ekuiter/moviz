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

function attachInputEvent(obj, cb) {
    obj.keypress(function(e) {
	if (e.which == 13)
            cb(obj.val());
    });
}

function instantPromise() {
    return $.Deferred().resolve().promise();
}
