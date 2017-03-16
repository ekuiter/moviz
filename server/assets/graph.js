function Graph() {
    var self = this;
    this.graphNodes = new GraphNodes();
    this.graphEdges = new GraphEdges();
    
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
	self.graphNodes.update();
	self.graphEdges.update();
	svgDocument().find("title").remove();
    });

    this.zoomingOrPanning = false;
    
    function zoomOrPanEvent() {
	if (!self.zoomingOrPanning) {
	    svgDocument().find(".node, #actors.edge text").each(function() {
		$(this).qtip("hide");
		$(this).qtip("disable");
	    });
	}
	self.zoomingOrPanning = true;
	defer(function() {
	    self.zoomingOrPanning = false;
	    svgDocument().find(".node, #actors.edge text").each(function() {
		$(this).qtip("enable");
	    });
	}, 500);
    }
}

Graph.prototype = {
    adjustPositionFn: function(anchor, num, denom) {
	num = num || 1;
	denom = denom || 1;
	return function(event, api) {
	    var offset = anchor.offset();
	    var x = offset.left + num * anchor.width() / denom;
	    var y = offset.top + anchor.height();
	    api.set("position.target", [x, y]);
	    api.reposition();
	    if (api.position.my.x === "right")
		x = offset.left + (denom - num) * anchor.width() / denom;
	    if (api.position.my.y === "bottom")
		y = offset.top;
	    api.set("position.target", [x, y]);
	}
    }
};
