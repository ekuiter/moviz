function NodeFilter(server) {
    if (!(this instanceof NodeFilter))
	return new NodeFilter(server);
    var self = this;
    
    self.nodes = [];
    self.server = server;
    self.server.getNodes().then(function(data) {
	if (data)
	    data.forEach(function(node) {
		self.nodes.push(node.title);
	    });
	self.update();
    });
};

NodeFilter.prototype = {
    invalidate: function() {
	var self = this;
	self.server.getNodes().then(function(data) {
	    $("#node-filter").empty();
	    if (data)
		data.forEach(function(node) {
		    $("#node-filter").append(self.constructNodeLabel(node));
		});
	});
    },

    constructNodeLabel: function(node) {
	return $("<label>").append($("<input type='checkbox'>").
				   prop("value", node.title).
				   prop("checked", this.nodes.indexOf(node.title) !== -1).
				   click(this.clickNode.bind(this))).append(node.title);
    },

    getCheckedNodes: function() {
	return $("#node-filter input:checkbox:checked").map(function() {
	    return $(this).val();
	}).get();
    },

    clickNode: function() {
	this.nodes = this.getCheckedNodes();
	this.update();
    },

    update: function() {
	this.server.filterNodes(["or-filter"].concat(this.nodes.map(function(node) {
	    return ["movie-filter", node];
	})));
    }
};
