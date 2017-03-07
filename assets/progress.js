function Progress(initialized) {
    var self = this;
    
    makeDialog("#progress-dialog", {
	dialogClass: "no-close",
	buttons: { "Abort": function() { self.abort(); } }
    });
    $("#progress-dialog .progress").progressbar();
    
    initialized();
}

Progress.prototype = {
    queue: function(fn, ms) {
	window.setTimeout(function() {
	    App().server.progress().then(fn);
	}, ms || 0);
    },
    
    report: function() {
	var self = this;
	var defer = $.Deferred();

	self.abort = function() {
	    $("#progress-dialog").dialog("close");
	    defer.reject();
	    App().server.abort();
	};
	
	self.queue(function fn(data) {
	    if (data !== null) {
		$("#progress-dialog").dialog("open");
		$("#progress-dialog .progress").progressbar("option", "value", data).
		    children(".ui-progressbar-value").
		    html(data.toFixed() + "%").css("display", "block");
		self.queue(fn, 1000);
	    } else {
		$("#progress-dialog").dialog("close");
		defer.resolve();
	    }
	});
	return defer.promise();
    }
};
