function MovieDialog(button, dialog, dialogButton, cb) {
    var self = this;    
    var options = { buttons: {} };
    options.buttons[dialogButton] = cb;
    var input = $(dialog).find("input");
    makeMenuDialog(button, dialog, options,
		   function() { input.val(""); });
    attachInputEvent(input, cb);
    input.autocomplete({
	source: function(req, res) {
	    var movies = self.extractMovies(req.term);
	    var moviesButLast = movies.slice(0, movies.length - 1);
	    if (movies.length && movies[movies.length - 1])
		App().server.suggest(movies[movies.length - 1]).fail(function() {
		    $("#error-dialog").dialog("close");
		    res();
		}).then(function(data) {
		    return data.map(function(movie) {
			return { label: movie, value: moviesButLast.concat(movie).join(", ") };
		    });
		}).then(res);
	    else
		res();
	}
    });
}

MovieDialog.prototype = {
    extractMovies: function(titles) {
	return titles.split(",").map(function(title) {
	    return title.trim();
	});
    }
};

function AddMovies() {
    var self = this;
    MovieDialog.call(self, "#add", "#add-dialog", "Add", addMovies);
    
    function addMovies() {
	var movies = self.extractMovies($("#add-dialog input").val());
	App().server.add(movies).always(function() {
	    $("#add-dialog").dialog("close");
	}).done(function() {
	    App().progress.report().done(function() {
		App().nodeFilter.checkAllFilters();
	    });
	});
    }
}

AddMovies.prototype = Object.create(MovieDialog.prototype);
AddMovies.prototype.constructor = AddMovies;

function Search() {
    var self = this;
    MovieDialog.call(self, "#search", "#search-dialog", "Search", search);
    
    function search() {
	$("#search-dialog").dialog("close");
	var movies = self.extractMovies($("#search-dialog input").val());
	if (movies.length)
	    new MovieDetails(movies[movies.length - 1]);
    }
}

Search.prototype = Object.create(MovieDialog.prototype);
Search.prototype.constructor = Search;
