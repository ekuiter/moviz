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
}
