var App = (function() {
    var self;
    
    return function App() {
	if (self)
	    return self;
	if (!(this instanceof App))
	    return new App();
	self = this;
	window.onerror = self.reportError;

	var initialize = (function() {
	    var initializing = [];

	    return function(props) {
		props.forEach(function(prop) {
		    var klass = eval(prop.substr(0, 1).toUpperCase() + prop.substr(1));
		    initializing.push(prop);
		    self[prop] = new klass(function() {
			initializing.splice(initializing.indexOf(prop), 1);
			if (initializing.length === 0)
			    allInitialized();
		    });
		});
	    }
	})();

	initialize(["server", "nodeFilter", "edgeFilter", "graphClasses", "sidebar"]);
	self.server.shouldInvalidate(self.nodeFilter);

	makeDialog("#error-dialog", {
	    dialogClass: "no-close error-dialog",
	    buttons: { "Okay": function() { $(this).dialog("close"); } }
	});

	$("#empty-graph .add").click(function() {
	    $("#add").click();
	});

	$("#empty-graph .all").click(function() {
	    $("#node-filter .all").click();
	});

	function allInitialized() {
	    defer(function() {
		$("body").css("opacity", 1);
	    });
	}
    }
})();

App.prototype.reportError = function(err) {
    $("#error-dialog").text(err).dialog("open");
};

$(document).ready(App);
