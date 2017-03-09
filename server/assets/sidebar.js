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
}
