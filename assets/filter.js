function Filter(server, sel) {
    if (!(this instanceof Filter))
	return new Filter(server);
    var self = this;

    self.checkedFilters = [];
    self.server = server;
    self.sel = sel;
}

Filter.prototype = {
    constructFilterLabel: function(name, text) {
	var self = this;
	var label = $("<label>");
	var input = $("<input type='checkbox'>").
	    prop("value", name).
	    prop("checked", this.checkedFilters.indexOf(name) !== -1).
	    click(self.clickFilter.bind(self));
	return label.append(input).append(text || name);
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
	$(self.sel).controlgroup({ direction: "vertical" }).
	    parent().controlgroup({ direction: "vertical" });
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
		return this.select = $("<select>").
		    append($("<option value='gender'>").append("Gender")).
		    append($("<option value='male'>").append("Male")).
		    append($("<option value='female'>").append("Female")).
		    on("selectmenuchange", self.clickFilter.bind(self));
	    },
	    getChecked: function() { return this.select.val() !== "gender"; },
	    update: function() { return ["gender-filter", this.select.val()]; }
	},

	"same-character": {
	    construct: function() {
		return self.constructFilterLabel("same-character", "Same character");
	    },
	    update: function() { return ["same-character-filter"]; }
	},

	billing: {
	    construct: function() {
		var input = this.input = $("<input>").addClass("ui-spinner-input").
		    prop("placeholder", "Billing");
		attachInputEvent(input, self.clickFilter.bind(self));
		return input;
	    },
	    afterConstruct: function() { this.input.spinner("option", "min", 1); },
	    getChecked: function() { return this.input.spinner("value"); },
	    update: function() { return ["billing-filter", this.input.spinner("value")]; }
	}
    };

    self.forFilters(function(filterObj) {
	$(self.sel).append(filterObj.construct.call(filterObj));
    });
    $(self.sel).controlgroup({ direction: "vertical" });
    self.forFilters(function(filterObj) {
	filterObj.afterConstruct && filterObj.afterConstruct.call(filterObj);
    });
    self.update();
};

EdgeFilter.prototype = Object.create(Filter.prototype);
EdgeFilter.prototype.constructor = EdgeFilter;

EdgeFilter.prototype.forFilters = function(cb) {
    var self = this;
    Object.getOwnPropertyNames(self.availableFilters).forEach(function(filterName) {
	var filterObj = self.availableFilters[filterName];
	cb(filterObj, filterName);
    });
};

EdgeFilter.prototype.getCheckedFilters = function() {
    var self = this;
    var checkedFilters = Filter.prototype.getCheckedFilters.call(this);
    self.forFilters(function(filterObj, filterName) {
	if (filterObj.getChecked && filterObj.getChecked.call(filterObj))
	    checkedFilters.push(filterName);
    });
    return checkedFilters;
};

EdgeFilter.prototype.update = function() {
    var self = this;
    self.server.filterEdges(["and-filter"].concat(this.checkedFilters.map(function(filter) {
	var filterObj = self.availableFilters[filter];
	return filterObj.update.call(filterObj);
    })));
};
