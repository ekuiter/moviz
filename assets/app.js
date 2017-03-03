var App = (function() {
    var self;
    
    return function App() {
	if (self)
	    return self;
	if (!(this instanceof App))
	    return new App();
	self = this;
	window.onerror = alert;
	
	self.server = new Server();
	self.nodeFilter = new NodeFilter(self.server);
	self.server.setNodeFilter(self.nodeFilter);

	attachInputEvent("#update", function(text) {
	    self.server.update(text.split("/"));
	});
	attachInputEvent("#add", function(text) {
	    self.server.add(text.split("/"));
	});
	attachInputEvent("#filter-nodes", function(text) {
	    self.server.filterNodes(JSON.parse(text));
	});
	attachInputEvent("#filter-edges", function(text) {
	    self.server.filterEdges(JSON.parse(text));
	});
}
})();

$(document).ready(App);
