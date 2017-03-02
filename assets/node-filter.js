var NodeFilter = function() {
    var self = this;
    self.nodes = [];
    app.server.getNodes().then(function(data) {
	if (data)
	    data.forEach(function(node) {
		self.nodes.push(node.title);
	    });
	self.update();
    });
};

NodeFilter.prototype.invalidate = function() {
    var self = this;
    app.server.getNodes().then(function(data) {
	$("#node-filter").empty();
	if (data)
	    data.forEach(function(node) {
		$("#node-filter").append(
		    $("<label>").append($("<input type='checkbox'>").
					prop("value", node.title).
					prop("checked", self.nodes.indexOf(node.title) !== -1).
					click(self.clickNode.bind(self))).
			append(node.title));
	    });
    });
};

NodeFilter.prototype.getCheckedNodes = function() {
    return $("#node-filter input:checkbox:checked").map(function() {
	return $(this).val();
    }).get();
};

NodeFilter.prototype.clickNode = function() {
    this.nodes = this.getCheckedNodes();
    this.update();
};

NodeFilter.prototype.update = function() {
    app.server.filterNodes(["or-filter"].concat(this.nodes.map(function(node) {
	return ["movie-filter", node];
    })));
};
