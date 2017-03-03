function Filter(server, sel) {
    if (!(this instanceof Filter))
	return new Filter(server);
    var self = this;

    self.checkedFilters = [];
    self.server = server;
    self.sel = sel;
}

Filter.prototype = {
    constructFilterLabel: function(name, control, cb) {
	var self = this;
	var label = $("<label>");
	var input = $("<input type='checkbox'>").
	    prop("value", name).
	    prop("checked", this.checkedFilters.indexOf(name) !== -1).
	    click(function() {
		var checked = label.find("input:checkbox:checked").length == 1;
		(cb || self.clickFilter.bind(self))(checked);
	    });
	return label.append(input).append(name).append(control);
    },

    getCheckedFilters: function() {
	return $(this.sel).find("input:checkbox:checked").map(function() {
	    return $(this).val();
	}).get();
    },

    clickFilter: function() {
	this.checkedFilters = this.getCheckedFilters();
	this.update();
    }
};

function NodeFilter(server) {
    if (!(this instanceof NodeFilter))
	return new NodeFilter(server);
    var self = this;
    Filter.call(self, server, "#node-filter .filters");

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
    return self.server.getNodes().then(function(data) {
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
    });
};

NodeFilter.prototype.update = function() {
    this.server.filterNodes(["or-filter"].concat(this.checkedFilters.map(function(node) {
	return ["movie-filter", node];
    })));
};

function EdgeFilter(server) {
    if (!(this instanceof EdgeFilter))
	return new EdgeFilter(server);
    var self = this;
    Filter.call(self, server, "#edge-filter");

    self.availableFilters = {
	gender: {
	    construct: function() {
		var select = this.select = $("<select disabled>").
		    append($("<option value='male'>").append("male")).
		    append($("<option value='female'>").append("female")).
		    change(self.clickFilter.bind(self));
		return self.constructFilterLabel("gender", select, function(checked) {
		    select.prop("disabled", !checked);
		    self.clickFilter();
		});
	    },

	    update: function() { return ["gender-filter", this.select.val()]; }
	},

	"same-character": {
	    construct: function() { return self.constructFilterLabel("same-character"); },
	    update: function() { return ["same-character-filter"]; }
	},

	billing: {
	    construct: function() {
		var input = this.input = $("<input disabled>").
		    prop("value", "10").change(self.clickFilter.bind(self));
		return self.constructFilterLabel("billing", input, function(checked) {
		    input.prop("disabled", !checked);
		    self.clickFilter();
		});
	    },

	    update: function() { return ["billing-filter", parseInt(this.input.val())]; }
	}
    };

    Object.getOwnPropertyNames(self.availableFilters).forEach(function(filter) {
	var filterObj = self.availableFilters[filter];
	$(self.sel).append(filterObj.construct.call(filterObj));
    });
    self.update();
};

EdgeFilter.prototype = Object.create(Filter.prototype);
EdgeFilter.prototype.constructor = EdgeFilter;

EdgeFilter.prototype.update = function() {
    var self = this;
    self.server.filterEdges(["and-filter"].concat(this.checkedFilters.map(function(filter) {
	var filterObj = self.availableFilters[filter];
	return filterObj.update.call(filterObj);
    })));
};
