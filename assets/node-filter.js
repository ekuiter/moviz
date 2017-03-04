function NodeFilter() {
    if (!(this instanceof NodeFilter))
	return new NodeFilter();
    var self = this;
    Filter.call(self, "#node-filter .filters");

    $("#node-filter .all").click(function() {
	self.checkedFilters = self.allNodes;
	self.update();
    });

    $("#node-filter .none").click(function() {
	self.checkedFilters = [];
	self.update();
    });

    self.getNodes().then(function() {
	$("#node-filter .all").click();
    });
};

NodeFilter.prototype = Object.create(Filter.prototype);
NodeFilter.prototype.constructor = NodeFilter;

NodeFilter.prototype.getNodes = function() {
    var self = this;
    return App().server.getNodes().then(function(data) {
	self.allNodes = [];
	if (data)
	    data.forEach(function(node) {
		self.allNodes.push(node.title);
	    });
	return self.allNodes;
    });
};

NodeFilter.prototype.invalidate = function() {
    var self = this;
    self.getNodes().then(function(nodes) {
	$(self.sel).empty();
	nodes.forEach(function(node) {
	    $(self.sel).append(self.constructFilterLabel(node));
	});
	$(self.sel).parent().controlgroup({ direction: "vertical" }).
	    find(".buttons").controlgroup();
	$(self.sel).controlgroup({ direction: "vertical" });
    });
};

NodeFilter.prototype.update = function() {
    App().server.filterNodes(["or-filter"].concat(this.checkedFilters.map(function(node) {
	return ["movie-filter", node];
    })));
};
