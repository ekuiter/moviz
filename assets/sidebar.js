function Sidebar() {
    $("#menu").menu({ items: "> :not(.ui-widget-header)" });
    
    $("#clear").click(function() {
	App().server.clear();
    });

    $("#collapse").click(function() {
	if ($("#sidebar").hasClass("collapsing"))
	    return;
	$("#sidebar").addClass("collapsing");
	$("#sidebar").scrollTop(0);
	$(this).find(".ui-icon").
	    toggleClass("ui-icon-triangle-1-n").
	    toggleClass("ui-icon-triangle-1-s");
	var duration = 150;
	var shouldHide = $(this).find(".text").text() === "Hide";
	$(this).find(".text").text(shouldHide ? "Show" : "Hide");
	
	window.setTimeout(function() {
	    $("#sidebar").height(shouldHide ? $("#menu").height() + 2 : "100%");
	}, shouldHide ? 0 : duration);
	window.setTimeout(function() {
	    $("#sidebar").toggleClass("collapsed");
	}, shouldHide ? duration : 0);
	window.setTimeout(function() {
	    $("#sidebar").removeClass("collapsing");
	}, duration);
    });	

    makeMenuDialog("#info", "#info-dialog");
    makeMenuDialog("#add", "#add-dialog", { buttons: { "Add": addMovies } });
    attachInputEvent($("#add-dialog input"), addMovies);
    $("#add-dialog input").autocomplete({
	source: function(req, res) {
	    var movies = extractMovies(req.term);
	    var moviesButLast = movies.slice(0, movies.length - 1);
	    if (movies.length && movies[movies.length - 1])
		App().server.suggest(movies[movies.length - 1]).then(function(data) {
		    return asList(data).map(function(movie) {
			return { label: movie, value: moviesButLast.concat(movie).join(", ") };
		    });
		}).then(res);
	    else
		res();
	}
    });

    function extractMovies(titles) {
	return titles.split(",").map(function(title) {
	    return title.trim();
	});
    }
    
    function addMovies() {
	App().server.add(extractMovies($("#add-dialog input").val()));
	$("#add-dialog input").val("");
	$("#add-dialog").dialog("close");
    }
}
