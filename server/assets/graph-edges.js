function GraphEdges() {
    var self = this;
    
    App().server.shouldInvalidateOnDestruction(this);
    self.invalidate();
}

GraphEdges.prototype = {
    update: function() {
	var self = this;
	svgDocument().find("#actors.edge text").each(function() {
	    var actor = $(this).text();
	    var nodes = $(this).siblings("title").text().split("--");
	    new RoleEdge(actor).prepareTooltip(this, nodes);
	});
    },

    invalidate: function() {
	App().server.cache.edges = null;
    }
};

function RoleEdge(obj) {
    if (!(this instanceof RoleEdge))
	return new RoleEdge(obj);
    if (typeof obj === "object")
	$.extend(this, obj);
    else
	this.role1 = { actor: { readableName: obj } };
}

RoleEdge.prototype = {
    getMetadata: function() {
	return App().server.tmdbSearch("actors", this.role1.actor.lastName +
				       ", " + this.role1.actor.firstName);
    },

    roleHtml: function(role, insert) {
	var role1Less = !this.role2 || billingScore(this.role1) < billingScore(this.role2);
	if (role == 1)
	    role = role1Less ? this.role1 : this.role2;
	else
	    role = role1Less ? this.role2 : this.role1;
	var pronoun = this.gender === "male" ? "He" : "She";
	var text = "";
	text += "<p>" + pronoun + insert + " plays <b>";
	if (billingScore(role) <= 3)
	    text += "<span class='ui-icon ui-icon-star'></span>";
	return text + (role.name || "?") + "</b> in <b>" + role.movie.title + "</b></p>";
    },

    edgeHtml: function(metadata) {
	var text = "";
	if (metadata && metadata.profileUrl)
	    text += "<img src='" + metadata.profileUrl + "' >";
	else if (metadata)
	    text += "<div class='image'><p>No image available.</p></div>";
	else
	    text += "<div class='image'><div class='loading'></div></div>";
	text += this.roleHtml(1, "");
	if (this.role2)
	    text += "<hr>" + this.roleHtml(2, " also");
	return text;
    },

    textHtml: function(actor, nodes, defer) {
	var self = this;
	defer = defer || $.Deferred();
	if (nodes && !App().server.cache.edges) {
	    defer.notify("<div class='loading'></div>");
	    App().server.getEdges().then(function() {
		return self.textHtml(actor, nodes, defer);
	    });
	} else {
	    if (!nodes)
		var edge = this;
	    else
		var edge = App().server.cache.edges.find(function(edge) {
		    return edge.node1.title === nodes[0] && edge.node2.title === nodes[1]
			&& edge.role1.actor.readableName === actor;
		});
	    if (!edge)
		return "Something went wrong. Try restarting moviz.";

	    defer.notify(edge.edgeHtml());
	    edge.getMetadata().then(function(metadata) {
		defer.resolve(edge.edgeHtml(metadata));
	    });
	}
	return defer.promise();
    },

    prepareTooltip: function(elem, nodes, showOnReady) {
	var self = this;
	var actor = this.role1.actor.readableName;

	$(elem).qtip({
	    content: {
		title: function(event, api) {
		    return actor;
		},
		text: function(event, api) {
		    return self.textHtml(actor, nodes);
		}
	    },
	    show: { solo: true, ready: showOnReady },
	    style: { classes: "qtip-dark qtip-shadow tooltip edge-tooltip" },
	    position: { my: "top left", viewport: true },
	    events: {
		show: $(elem).parents("svg").length ?
		    App().graph.adjustPositionFn($(elem), 3, 4) :
		    null
	    }
	});
    }
};
