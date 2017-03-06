function Sidebar() {
    $("#menu").menu({ items: "> :not(.ui-widget-header)" });

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
	
	defer(function() {
	    $("#sidebar").height(shouldHide ? $("#menu").height() + 2 : "100%");
	}, shouldHide ? 0 : duration);
	defer(function() {
	    $("#sidebar").toggleClass("collapsed");
	}, shouldHide ? duration : 0);
	defer(function() {
	    $("#sidebar").removeClass("collapsing");
	}, duration);
    });	

    makeMenuDialog("#info", "#info-dialog");

    makeMenuDialog("#clear", "#clear-dialog", { buttons: { "Yes": function() {
	App().server.clear();
	$("#clear-dialog").dialog("close");
    } } });

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
	App().server.add(movies).then(function() {
	    movies.forEach(function(movie) {
		App().nodeFilter.checkFilter(movie);
	    });
	    App().nodeFilter.update();
	});
	$("#add-dialog").dialog("close");
    }

    makeMenuDialog("#debug", "#debug-dialog", { buttons: { "Eval": eval } }, function() {
	$("#debug-dialog input").val("");
	$("#debug-dialog .results").text("");
    });
    attachInputEvent($("#debug-dialog input"), eval);
    $("#debug-dialog .progress").progressbar({ value: false });

    function eval() {
	var input = $("#debug-dialog input");
	var progress = $("#debug-dialog .progress");
	var results = $("#debug-dialog .results");
	if (input.val().trim()) {
	    input.prop("disabled", true);
	    progress.show();
	    results.hide();
	    App().server.eval(input.val()).always(function() {
		input.prop("disabled", false);
		progress.hide();
	    }).then(function(data) {
		input.focus();
		results.show();
		$("#debug-dialog .results").text(JSON.stringify(data));
	    });
	}
    }
}
