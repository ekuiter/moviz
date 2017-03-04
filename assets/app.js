var App = (function() {
    var self;
    
    return function App() {
	if (self)
	    return self;
	if (!(this instanceof App))
	    return new App();
	self = this;
	window.onerror = self.reportError;
	
	self.server = new Server();
	self.nodeFilter = new NodeFilter();
	self.edgeFilter = new EdgeFilter();
	self.graphClasses = new GraphClasses();
	self.server.shouldInvalidate(self.nodeFilter);

	$("#menu").menu({ items: "> :not(.ui-widget-header)" });
	
	$("#clear").click(function() {
	    self.server.clear();
	});

	makeDialog("#error-dialog", {
	    dialogClass: "no-close error-dialog",
	    buttons: { "Okay": function() { $(this).dialog("close"); } }
	});
	makeMenuDialog("#info", "#info-dialog");
	makeMenuDialog("#add", "#add-dialog", { buttons: { "Add": addMovie } });
	attachInputEvent($("#add-dialog input"), addMovie);
	$("#add-dialog input").autocomplete({
	    source: function(req, res) { self.server.suggest(req.term).then(res); }
	});
	
	function addMovie() {
	    self.server.add($("#add-dialog input").val().split("/"));
	    $("#add-dialog").dialog("close");
	}

	$("body").show();
    }
})();

App.prototype.reportError = function(err) {
    $("#error-dialog").text(err).dialog("open");
};

$(document).ready(App);
