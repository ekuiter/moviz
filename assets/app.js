var app = {};

$(function() {
    window.onerror = alert;
    app.server = new Server();
    app.nodeFilter = new NodeFilter();
    
    attachInputEvent("#update", function(text) {
	app.server.update(text.split("/"));
    });
    attachInputEvent("#add", function(text) {
	app.server.add(text.split("/"));
    });
    attachInputEvent("#filter-nodes", function(text) {
	app.server.filterNodes(JSON.parse(text));
    });
    attachInputEvent("#filter-edges", function(text) {
	app.server.filterEdges(JSON.parse(text));
    });
});
