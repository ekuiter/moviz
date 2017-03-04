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
	self.nodeFilter = new NodeFilter(self.server);
	self.server.shouldInvalidate(self.nodeFilter);
	self.edgeFilter = new EdgeFilter(self.server);

	$("#menu").menu({ items: "> :not(.ui-widget-header)" });
	$("#info").click(function() {
	    $("#info-dialog").dialog("open");
	});
	$("#clear").click(function() {
	    self.server.clear();
	});

	$("#info-dialog").dialog({ autoOpen: false, modal: true, width: 400 });
	$("#error-dialog").dialog({
	    autoOpen: false, modal: true, width: 400,
	    dialogClass: "no-close error-dialog",
	    buttons: {
		"Okay": function() {
		    $(this).dialog("close");
		}
	    }
	});

	attachInputEvent($("#add"), function(text) {
	    self.server.add(text.split("/"));
	});
	$("#add").autocomplete({
	    source: function(req, res) {
		self.server.suggest(req.term).then(res);
	    }
	});
	attachInputEvent($("#update"), function(text) {
	    self.server.update(text.split("/"));
	});
	
	$("body").show();
    }
})();

App.prototype.reportError = function(err) {
    $("#error-dialog").text(err).dialog("open");
};

$(document).ready(App);
