function MovieDialog(button, dialog, dialogButton, cb, cb2) {
    var self = this;    
    var options = { buttons: {} };
    options.buttons[dialogButton] = cb;
    var input = $(dialog).find("input");
    makeMenuDialog(button, dialog, options,
		   function() { input.val(""); if (cb2) cb2(); });
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
    MovieDialog.call(self, "#add", "#add-dialog", "Add", addMovies, function() {
        var div = $("#add-dialog .options").empty();
        var options = self.options = new Filter(div);
        var imdbLabel = options.constructFilterLabel("imdb", "Add IMDb actor information");
        div.append(imdbLabel).
            append(options.constructFilterLabel("synchronkartei",
                                                "Add German voice actor information")).
            controlgroup({ direction: "vertical" });
        imdbLabel.find("input").click();
    });
    
    function addMovies() {
        var defer = instantPromise();
        defer.always(function() {
	    $("#add-dialog").dialog("close");
	});
	var movies = self.extractMovies($("#add-dialog input").val()).
            filter(function(movie) { return movie !== ""; });
        
	if (movies.length > 0 && self.options.isChecked("imdb")) {
            defer = App().server.add(movies).done(function() {
	        App().progress.report().done(function() {
		    App().nodeFilter.checkAllFilters().then(function() {
                        if (self.options.isChecked("synchronkartei"))
                            App().addVoiceActors.update();
                    });
	        });
	    });
        } else if (self.options.isChecked("synchronkartei"))
            App().addVoiceActors.update();
        
        return defer.promise();
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
