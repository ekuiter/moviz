function NodeFilter() {
    if (!(this instanceof NodeFilter))
	return new NodeFilter();
    var self = this;
    Filter.call(self, "#node-filter .filters");

    $("#node-filter").prepend(self.constructFilterLabel("neighborhood-filter", "Neighborhood"));

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
	asList(data).forEach(function(node) {
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

NodeFilter.prototype.getCheckedFilters = function() {
    return Filter.prototype.getCheckedFilters.call(this, "#node-filter");
};

NodeFilter.prototype.update = function() {
    var nhFilter = "neighborhood-filter";
    var filterType = this.checkedFilters.indexOf(nhFilter) !== -1 ? nhFilter : "movie-filter";
    App().server.filterNodes(["or-filter"].concat(
	this.checkedFilters.
	    filter(function(node) { return node !== nhFilter; }).
	    map(function(node) {
		return [filterType, node];
	    })));
};
