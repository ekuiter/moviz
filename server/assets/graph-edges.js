function GraphEdges() {
    var self = this;
    
    App().server.shouldInvalidateOnDestruction(this);
    self.invalidate();
}

GraphEdges.prototype = {
    update: function() {
	svgDocument().find("#actors.edge text").each(function() {
	    var self = this;
	    var actor = $(this).text();
	    var nodes = $(this).siblings("title").text().split("--");

	    function roleHtml(edge, role, insert) {
		if (role == 1)
		    role = edge.role1.billing < edge.role2.billing ? edge.role1 : edge.role2;
		else
		    role = edge.role1.billing < edge.role2.billing ? edge.role2 : edge.role1;
		var pronoun = edge.gender === "male" ? "He" : "She";
		var text = "";
		text += "<p>" + pronoun + insert + " plays <b>";
		if (role.billing && role.billing <= 3)
		    text += "<span class='ui-icon ui-icon-star'></span>";
		return text + (role.name || "?") + "</b> in <b>" + role.movie.title + "</b></p>";
	    }

	    function edgeHtml(edge, metadata) {
		var text = "";
		if (metadata && metadata.profileUrl)
		    text += "<img src='" + metadata.profileUrl + "' >";
		else if (metadata)
		    text += "<div class='image'><p>No image available.</p></div>";
		else
		    text += "<div class='image'><div class='loading'></div></div>";
		return text + roleHtml(edge, 1, "") + "<hr>" + roleHtml(edge, 2, " also");
	    }

	    function textHtml(defer) {
		defer = defer || $.Deferred();
		if (!App().server.cache.edges) {
		    defer.notify("<div class='loading'></div>");
		    App().server.getEdges().then(function() {
			return textHtml(defer);
		    });
		} else {
		    var edge = App().server.cache.edges.find(function(edge) {
			return edge.node1.title === nodes[0] && edge.node2.title === nodes[1]
			    && edge.role1.actor.readableName === actor;
		    });
		    if (!edge)
			return "Something went wrong. Try restarting moviz.";

		    defer.notify(edgeHtml(edge));
		    edge.getMetadata().then(function(metadata) {
			defer.resolve(edgeHtml(edge, metadata));
		    });
		}
		return defer.promise();
	    }

	    $(this).qtip({
		content: {
		    title: function(event, api) {
			return actor;
		    },
		    text: function(event, api) {
			return textHtml();
		    }
		},
		show: { solo: true },
		style: { classes: "qtip-dark qtip-shadow tooltip edge-tooltip" },
		position: { my: "top left", viewport: true },
		events: { show: App().graph.adjustPositionFn($(this)) }
	    });
	});
    },

    invalidate: function() {
	App().server.cache.edges = null;
    }
};

function Actor(obj) {
    if (!(this instanceof Actor))
	    return new Actor(obj);
    $.extend(this, obj);
}

Actor.prototype = {
    getMetadata: function() {
	return App().server.tmdbSearch("actors", this.role1.actor.lastName +
				       ", " + this.role1.actor.firstName);
    }
};
