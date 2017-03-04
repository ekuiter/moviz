function EdgeFilter() {
    if (!(this instanceof EdgeFilter))
	return new EdgeFilter();
    var self = this;
    Filter.call(self, "#edge-filter");

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
    var checkedFilters = Filter.prototype.getCheckedFilters.call(self);
    self.forFilters(function(filterObj, filterName) {
	if (filterObj.getChecked && filterObj.getChecked.call(filterObj))
	    checkedFilters.push(filterName);
    });
    return checkedFilters;
};

EdgeFilter.prototype.update = function() {
    var self = this;
    App().server.filterEdges(["and-filter"].concat(this.checkedFilters.map(function(filter) {
	var filterObj = self.availableFilters[filter];
	return filterObj.update.call(filterObj);
    })));
};
