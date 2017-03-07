function Debug() {
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
