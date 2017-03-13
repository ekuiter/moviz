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

function instantPromise(arg) {
    return $.Deferred().resolve(arg).promise();
}

function asList(obj) {
    return obj === null ? [] : obj;
}

function asObject(obj) {
    return obj === null ? {} : obj;
}

function makeDialog(sel, options) {
    $(sel).dialog($.extend({
	autoOpen: false, modal: true, width: 400,
	show: { effect: "fadeIn", duration: 150 },
	hide: { effect: "fadeOut", duration: 150 }
    }, options));
}

function makeMenuDialog(buttonSel, dialogSel, options, cb) {
    makeDialog(dialogSel, options);
    $(buttonSel).click(function() {
	$(dialogSel).dialog("open");
	if (cb)
	    cb();
    });
}

function defer(fn, ms) {
    return window.setTimeout(fn, ms || 0);
}

function withElectron() {
    if (window.require)
	return $.Deferred().resolve(window.require("electron")).promise();
    else
	return $.Deferred().reject().promise();
}

function withElectronGlobal(name) {
    return withElectron().then(function(electron) {
	return electron.remote.getGlobal(name);
    });
}

function callElectronGlobal(name) {
    var args = Array.prototype.slice.call(arguments, 1);
    return withElectronGlobal(name).then(function(fn) {
	return fn.apply(null, args);
    });
}

function svgDocument() {
    return $($("#graph")[0].contentDocument);
}
