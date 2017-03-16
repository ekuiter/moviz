function MovieDetails(movie) {
    if (!(movie instanceof Movie))
	movie = new Movie(movie);
    
    var dialog = $("#movie-details-dialog");

    function makeMovieDetailsDialog() {
	makeDialog(dialog, {
	    width: "90vw", height: $(window).height() * 0.9, resizable: false
	});
    }

    makeMovieDetailsDialog();
    $(window).resize(makeMovieDetailsDialog);

    function attachLinks() {
	dialog.find(".details a.episode").click(function(e) {
	    var self = this;
	    e.preventDefault();
	    var episodeLink = dialog.find(".episodes li").filter(function() {
		return $(this).text() === $(self).text();
	    }).find("a").click();
	});
	dialog.find(".details a.movie").each(function() {
	    var movie = new Movie($(this).text().match(/"?(.*?)"? \(.{4}\)/)[1]);
	    movie.prepareTooltip(this);
	    
	    $(this).click(function(e) {
		e.preventDefault();
		$(this).qtip("hide");
		new MovieDetails(movie);
	    });
	});
    }

    function readableList(list) {
	var map = {
	    trivia: "Trivia", goofs: "Goofs", quotes: "Quotes", soundtracks: "Soundtracks",
	    crazyCredits: "Crazy Credits", alternateVersions: "Alternate Versions"
	};
	return map[list];
    }

    function emify(match, information) {
	return "<em>" + information + "</em>";
    }
    
    function infoHtml(record) {
	var rules = [
	    [/'([^']*?)' \(qv\)/g, emify],
	    [/_([^_]*?)( \{([^{]*)\})?_ \(qv\)/g, function(match, currentMovie, _, episode) {
		if (currentMovie.indexOf(movie.title) !== -1 && episode)
		    return "<a href='#' class='episode'><em>" + episode + "</em></a>";
		else
		    return "<a href='#' class='movie'><em>" + currentMovie + "</em></a>" +
		(episode ? " - <em>" + episode + "</em>" : "");
	    }],
	    [/SPOILER:/g, "<span class='spoiler'>Spoiler</span>:"],
	    [/^CONT:/g, "<span class='goof'>Continuity</span>:"],
	    [/^PLOT:/g, "<span class='goof'>Plot hole</span>:"],
	    [/^FACT:/g, "<span class='goof'>Factual error</span>:"],
	    [/^CHAR:/g, "<span class='goof'>Character error</span>:"],
	    [/^FAKE:/g, "<span class='goof'>Fake</span>:"],
	    [/^SYNC:/g, "<span class='goof'>Audio/visual mismatch</span>:"],
	    [/^BOOM:/g, "<span class='goof'>Boom mic visible</span>:"],
	    [/^CREW:/g, "<span class='goof'>Crew visible</span>:"],
	    [/^DATE:/g, "<span class='goof'>Anachronism</span>:"],
	    [/^GEOG:/g, "<span class='goof'>Geography error</span>:"]
	];
	var quoteRules = [
	    [/^(\w.*?):/gm, function(match, name) {
		return "</div><div><span class='quote'>" + name + "</span>" + ":";
	    }],
	    [/\n  /gm, " "],
	    [/(\[[^\[]*?\])/gm, emify]
	];

	function applyRules(info, rules) {
	    return rules.reduce(function(info, rule) {
		return info.replace(rule[0], rule[1]);
	    }, info);
	}
	
	return record.info.map(function(info) {
	    info = applyRules(info, rules);
	    if (record.prototype.lispClass === "quotes")
		info = "<div>" + applyRules(info, quoteRules) + "</div>";
	    info = info.replace(/\n/g, "<br>");
	    return $("<li>").addClass(record.prototype.lispClass).html(info);
	});
    }

    this.assertMovie(movie).then(function() {
	dialog.dialog("option", "title", "Movie: " + movie.title).dialog("open").empty().
	    append($("<div class='loading-big'>"));
	App().server.details(movie.title).then(function(data) {
	    var episodesUl = $("<ul class='episodes'>");
	    var detailsDiv = $("<div class='details'>");
	    dialog.empty().append(episodesUl).append(detailsDiv);
	    episodesUl.append($("<p>").append($("<b>").text("Episodes")));
	    asList(data.details).forEach(function(records) {
		var episode = records[0].episode || movie.title;
		episodesUl.append(
		    $("<li>").append($("<a href='#'>").text(episode).click(function(e) {
			e.preventDefault();
			episodesUl.find("li").removeClass("active");
			$(this).parent().addClass("active");
			detailsDiv.empty().scrollTop(0).append($("<h3>").text(episode));
			records.forEach(function(record) {
			    detailsDiv.append(
				$("<b>").text(readableList(record.prototype.lispClass))).
				append($("<ul>").append(infoHtml(record)));
			});
			attachLinks();
		    })));
	    });
	    if (data.details)
		episodesUl.find("li a").first().click();
	    else
		episodesUl.append($("<p>").text("No episodes found."));
	});
    }, function() {
	App().reportError("movie " + movie.title + " not found");
    });
}

MovieDetails.prototype = {    
    assertMovie: function(movie) {
	var defer = $.Deferred();
	App().server.suggest(movie.title).then(function(data) {
	    if (data && data.indexOf(movie.title) !== -1)
		defer.resolve();
	    else
		defer.reject();
	});
	return defer.promise();
    }
};
