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
	self.sidebar = new Sidebar();
	self.server.shouldInvalidate(self.nodeFilter);

	makeDialog("#error-dialog", {
	    dialogClass: "no-close error-dialog",
	    buttons: { "Okay": function() { $(this).dialog("close"); } }
	});

	$("body").show();
    }
})();

App.prototype.reportError = function(err) {
    $("#error-dialog").text(err).dialog("open");
};

$(document).ready(App);
