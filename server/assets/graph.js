function Graph() {
    makeMenuDialog("#clear", "#clear-dialog", { buttons: { "Yes": function() {
	App().server.clear();
	$("#clear-dialog").dialog("close");
    } } });

    $("#save").click(function() {
	App().server.saveGraph();
    });

    makeMenuDialog("#load", "#load-dialog", { buttons: { "Load": load } }, function() {
	$("#load-dialog input").val("");
    });
    attachInputEvent($("#load-dialog input"), load);
    $("#load-dialog .progress").progressbar({ value: false });

    function load() {
	var input = $("#load-dialog input");
	var progress = $("#load-dialog .progress");
	if (input.val().trim()) {
	    input.prop("disabled", true);
	    progress.show();
	    App().server.loadGraph(input.val()).always(function() {
		input.prop("disabled", false);
		progress.hide();
	    }).done(function() {
		$("#load-dialog").dialog("close");
		App().nodeFilter.checkAllFilters();
	    });
	}
    }
}
