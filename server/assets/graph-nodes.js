function GraphNodes() {}

GraphNodes.prototype = {
    update: function() {
	svgDocument().find(".node").each(function() {
	    var self = this;
	    var movieTitle = $(this).children("text").text();
	    var movie = App().server.cache.nodes.find(function(node) {
		return node.title === movieTitle;
	    });
	    
	    $(this).qtip({
		content: {
		    title: function(event, api) {
			var title = "";
			title += movie.title;
			title += (movie.year ? " (" + movie.year + ")" : "");
			title += (movie.type ? " (" +
				  (movie.type === "series" ? "TV series" : "Movie") + ")" : "");
			return title;
		    },
		    text: function(event, api) {
			movie.getMetadata().then(function(metadata) {
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
			    api.set("content.text", text);
			});
			return "<div class='loading'></div>";
		    }
		},
		show: { solo: true },
		style: { classes: "qtip-dark qtip-shadow tooltip" },
		position: { my: "top left", adjust: { y: 5 }, viewport: true },
		events: { show: App().graph.adjustPositionFn($(this).children("text"), 3, 4) }
	    });
	});
    }
};
