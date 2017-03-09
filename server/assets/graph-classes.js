function GraphClasses(initialized) {
    if (!(this instanceof GraphClasses))
	return new GraphClasses();
    var self = this;
    Filter.call(self, "#graph-classes");

    self.checkedFilters = ["unlabeled", "weighted"];
    [["unlabeled", "Unlabeled"], ["condensed", "Condensed"],
     ["top-actors", "Top actors"], ["detailed", "Detailed"],
     ["weighted", "Weighted"]].forEach(function(arr) {
	$(self.sel).append(self.constructFilterLabel.apply(self, arr));
    });
    $(self.sel).controlgroup({ direction: "vertical" });
    self.update().then(initialized);
};

GraphClasses.prototype = Object.create(Filter.prototype);
GraphClasses.prototype.constructor = GraphClasses;

GraphClasses.prototype.update = function() {
    if (this.checkedFilters.length === 0)
	this.checkedFilters.push("unlabeled");
    return App().server.update(this.checkedFilters);
};
