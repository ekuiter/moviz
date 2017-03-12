function Graph() {
    makeMenuDialog("#clear", "#clear-dialog", { buttons: { "Yes": function() {
	App().server.clear();
	$("#clear-dialog").dialog("close");
    } } });

    $("#save").click(function() {
	App().server.saveGraph();
    });

    $("#export").click(function() {
	App().server.exportGraph();
    });

    makeMenuDialog("#load", "#load-dialog", { buttons: { "Load": loadFn(null) } }, function() {
	$("#load-dialog input").val("");
    });
    attachInputEvent($("#load-dialog input"), loadFn(null));
    $("#load-dialog .progress").progressbar({ value: false });

    withElectron().then(function(electron) {
	$("#load").unbind("click").click(function() {
	    var fileNames = electron.remote.dialog.showOpenDialog(
		electron.remote.getGlobal("win"), { properties: ["openFile"] });
	    loadFn(fileNames)();
	});
    });

    function loadFn(fileNames) {
	return function() {
	    var dialog = $("#load-dialog");
	    var input = $("#load-dialog input");
	    var progress = $("#load-dialog .progress");
	    if (fileNames !== null)
		input.val(fileNames || "");
	    if (input.val().trim()) {
		if (fileNames !== null)
		    dialog.dialog("open");
		input.prop("disabled", true);
		progress.show();
		App().server.loadGraph(input.val()).always(function() {
		    input.prop("disabled", false);
		    progress.hide();
		    if (fileNames !== null)
			dialog.dialog("close");
		}).done(function() {
		    dialog.dialog("close");
		    App().nodeFilter.checkAllFilters();
		});
	    }
	};
    }

    $("#graph").on("load", function() {	
	var panZoom = svgPanZoom("#graph", { fit: false, center: false,
					     onZoom: zoomOrPanEvent, onPan: zoomOrPanEvent }).
	    zoomBy(0.8).panBy({ x: 100, y: 0 });
	$(window).resize(function() {
	    panZoom.resize().fit().center().zoomBy(0.8).panBy({ x: 100, y: 0 });
	});

	var link = svgDocument()[0].createElementNS("http://www.w3.org/1999/xhtml", "link");
	svgDocument().find("svg").append($(link).prop("href", "graph.css").
					 prop("type", "text/css").prop("rel", "stylesheet"));
	new GraphNodes();
	new GraphEdges();
	svgDocument().find("title").remove();
    });
}

var zoomOrPanEvent = (function() {
    var zoomingOrPanning = false;

    return function() {
	if (!zoomingOrPanning) {
	    svgDocument().find(".node").each(function() {
		$(this).qtip("hide");
		$(this).qtip("disable");
	    });
	}
	zoomingOrPanning = true;
	defer(function() {
	    zoomingOrPanning = false;
	    svgDocument().find(".node").each(function() {
		$(this).qtip("enable");
	    });
	}, 500);
    };
})();

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
	text += "<p>" + movie.actors + " actors, " + movie.actresses + " actresses</p>";
	if (movie.plot)
	    text += "<p>" + movie.plot + "</p>";
	
	$(this).qtip({
	    content: { title: title, text: text },
	    style: { classes: "qtip-dark qtip-shadow node-tooltip" },
	    position: { my: "top left", adjust: { y: 5 } },
	    events: {
		show: function(event, api) {
		    var offset = textEl.offset();
		    api.set("position.target", [offset.left + 2 * textEl.width() / 3,
						offset.top + textEl.height()]);
		}
	    }
	});
    });
}

function GraphEdges() {
    
}
