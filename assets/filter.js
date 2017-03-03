function Filter(server, sel) {
    if (!(this instanceof Filter))
	return new Filter(server);
    var self = this;
    
    self.checkedFilters = [];
    self.server = server;
    self.sel = sel;
}

Filter.prototype = {
    constructFilterLabel: function(label, control, cb) {
	var self = this;
	var input = $("<input type='checkbox'>").
	    prop("value", label).
	    prop("checked", this.checkedFilters.indexOf(label) !== -1).
	    click(function() {
		var checked = $(self.sel + " ." + label + " input:checkbox:checked").length == 1;
		(cb || self.clickFilter.bind(self))(checked);
	    });
	return $("<label>").addClass(label).append(input).append(label).append(control);
    },

    getCheckedFilters: function() {
	return $(this.sel + " input:checkbox:checked").map(function() {
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
    Filter.call(self, server, "#node-filter");
    
    self.server.getNodes().then(function(data) {
	if (data)
	    data.forEach(function(node) {
		self.checkedFilters.push(node.title);
	    });
	self.update();
    });
};

NodeFilter.prototype = Object.create(Filter.prototype);
NodeFilter.prototype.constructor = NodeFilter;

NodeFilter.prototype.invalidate = function() {
    var self = this;
    self.server.getNodes().then(function(data) {
	$(self.sel).empty();
	if (data)
	    data.forEach(function(node) {
		$(self.sel).append(self.constructFilterLabel(node.title));
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
		var select = $("<select disabled>").
		    append($("<option value='male'>").append("male")).
		    append($("<option value='female'>").append("female")).
		    change(self.clickFilter.bind(self));
		return self.constructFilterLabel("gender", select, function(checked) {
		    $(self.sel + " .gender select").prop("disabled", !checked);
		    self.clickFilter();
		});
	    },

	    update: function() {
		return ["gender-filter", $(self.sel + " .gender select").val()];
	    }
	},

	"same-character": {
	    construct: function() { return self.constructFilterLabel("same-character"); },
	    update: function() { return ["same-character-filter"]; }
	},

	billing: {
	    construct: function() {
		var input = $("<input disabled>").addClass("billing-input").
		    prop("value", "10").change(self.clickFilter.bind(self));
		return self.constructFilterLabel("billing", input, function(checked) {
		    $(self.sel + " .billing-input").prop("disabled", !checked);
		    self.clickFilter();
		});
	    },

	    update: function() {
		return ["billing-filter", parseInt($(self.sel + " .billing-input").val())];
	    }
	}
    };

    Object.getOwnPropertyNames(self.availableFilters).forEach(function(filter) {
	$(self.sel).append(self.availableFilters[filter].construct());
    });
    self.update();
};

EdgeFilter.prototype = Object.create(Filter.prototype);
EdgeFilter.prototype.constructor = EdgeFilter;

EdgeFilter.prototype.update = function() {
    var self = this;
    self.server.filterEdges(["and-filter"].concat(this.checkedFilters.map(function(filter) {
	return self.availableFilters[filter].update();
    })));
};
