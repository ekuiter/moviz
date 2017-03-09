function AddMovies() {
    makeMenuDialog("#add", "#add-dialog", { buttons: { "Add": addMovies } },
		   function() { $("#add-dialog input").val(""); });
    attachInputEvent($("#add-dialog input"), addMovies);
    $("#add-dialog input").autocomplete({
	source: function(req, res) {
	    var movies = extractMovies(req.term);
	    var moviesButLast = movies.slice(0, movies.length - 1);
	    if (movies.length && movies[movies.length - 1])
		App().server.suggest(movies[movies.length - 1]).fail(function() {
		    $("#error-dialog").dialog("close");
		    res();
		}).then(function(data) {
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
	var movies = extractMovies($("#add-dialog input").val());
	App().server.add(movies).always(function() {
	    $("#add-dialog").dialog("close");
	}).done(function() {
	    App().progress.report().done(function() {
		App().nodeFilter.checkAllFilters();
	    });
	});
    }
}
