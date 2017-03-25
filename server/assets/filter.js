function Filter(sel) {
    if (!(this instanceof Filter))
	return new Filter();
    var self = this;

    self.checkedFilters = [];
    self.sel = sel;
}

Filter.prototype = {
    constructFilterLabel: function(name, text) {
	var self = this;
	var label = $("<label>");
	var input = $("<input type='checkbox'>").
	    prop("value", name).
	    prop("checked", this.isChecked(name)).
	    click(self.clickFilter.bind(self));
	return label.append(input).append(text || name);
    },

    getCheckedFilters: function(sel) {
	sel = sel || this.sel;
	return $(sel).find("input:checkbox:checked").map(function() {
	    return $(this).val();
	}).get();
    },

    clickFilter: function() {
	this.checkedFilters = this.getCheckedFilters();
        if (this.update)
	    this.update();
    },

    isChecked: function(filter) {
        return this.checkedFilters.indexOf(filter) !== -1;
    }
};
