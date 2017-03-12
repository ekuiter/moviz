function GraphNodes() {
    svgDocument().find(".node").each(function() {
	var self = this;
	var textEl = $(this).children("text");
	var movieTitle = $(this).children("text").text();
	var movie = App().server.cachedNodes.find(function(node) {
	    return node.title === movieTitle;
	});
	
	var title = "";
	title += movie.title;
	title += (movie.year ? " (" + movie.year + ")" : "");
	title += (movie.type ? " (" +
		  (movie.type === "series" ? "TV series" : "Movie") + ")" : "");
	var text = "";
	if (movie.posterUrl)
	    text += "<img src='" + movie.posterUrl + "' width='154' height='231'>";
	if (movie.genres)
	    text += "<p><b>" + movie.genres.join(", ") + "</b></p>";
	if (movie.plot)
	    text += "<p>" + movie.plot + "</p>";
	
	$(this).qtip({
	    content: { title: title, text: text },
	    style: { classes: "qtip-dark qtip-shadow node-tooltip" },
	    position: { my: "top left", adjust: { y: 5 }, viewport: true },
	    events: {
		show: function(event, api) {
		    var offset = textEl.offset();
		    var x = offset.left + 3 * textEl.width() / 4;
		    var y = offset.top + textEl.height();
		    api.set("position.target", [x, y]);
		    api.reposition();
		    if (api.position.my.x === "right")
			x = offset.left + 1 * textEl.width() / 4;
		    if (api.position.my.y === "bottom")
			y = offset.top;
		    api.set("position.target", [x, y]);
		}
	    }
	});
    });
}
