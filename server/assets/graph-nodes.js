function GraphNodes() {}

GraphNodes.prototype = {
    update: function() {
	svgDocument().find(".node").each(function() {
	    var self = this;
	    var movieTitle = $(this).children("text").text();
	    var movie = App().server.cache.nodes.find(function(node) {
		return node.title === movieTitle;
	    });
	    movie.prepareTooltip(this);
	    $(this).click(function() {
		if (!App().graph.zoomingOrPanning) {
		    $(this).qtip("hide");
		    new MovieDetails(movie);
		}
	    });
	});
    }
};

function Movie(obj) {
    if (!(this instanceof Movie))
	    return new Movie(obj);
    if (typeof obj === "object")
	$.extend(this, obj);
    else
	this.title = obj;
}

Movie.prototype = {
    getMetadata: function() {
	return App().server.tmdbSearch("movies", this.title);
    },

    textHtml: function() {
	var defer = $.Deferred();
	defer.notify("<div class='loading'></div>");
	this.getMetadata().then(function(metadata) {
	    var text = "";
	    if (metadata.posterUrl)
		text += "<img src='" + metadata.posterUrl + "'>";
	    if (metadata.genres)
		text += "<p><b>" + metadata.genres.join(", ") + "</b></p>";
	    if (metadata.plot)
		text += "<p>" + metadata.plot + "</p>";
	    if (text === "")
		text += "<p>No information found.</p>" +
		"<p>Check your Internet connection, then restart moviz.</p>";
	    defer.resolve(text);
	});
	return defer.promise();
    },

    prepareTooltip: function(elem) {
	var self = this;
	$(elem).qtip({
	    content: {
		title: function(event, api) {
		    var title = "";
		    title += self.title;
		    title += (self.year ? " (" + self.year + ")" : "");
		    title += (self.type ? " (" +
			      (self.type === "series" ? "TV series" : "Movie") + ")" : "");
		    return title;
		},
		text: function(event, api) {
		    return self.textHtml();
		}
	    },
	    show: { solo: true },
	    style: { classes: "qtip-dark qtip-shadow tooltip" },
	    position: { my: "top left", adjust: { y: 5 }, viewport: true },
	    events: {
		show: $(elem).parents("svg").length ?
		    App().graph.adjustPositionFn($(elem).children("text"), 3, 4) :
		    null
	    }
	});
    }
};
