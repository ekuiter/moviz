function Progress() {
    var self = this;
    
    makeDialog("#progress-dialog", {
	dialogClass: "no-close",
	buttons: { "Abort": function() { self.abort(); } }
    });
    $("#progress-dialog .progress").progressbar();
}

Progress.prototype = {
    queue: function(fn, ms) {
	defer(function() {
	    App().server.progress().then(fn);
	}, ms);
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
		$("#progress-dialog .progress").
		    progressbar("option", "value", data !== 100 && data).
		    children(".ui-progressbar-value").
		    html(data === 100 ? "Crunching data ..." : data.toFixed() + "%").
		    css("display", "block");
		callElectronGlobal("setProgressBar", data / 100);
		self.queue(fn, 1000);
	    } else {
		$("#progress-dialog").dialog("close");
		callElectronGlobal("setProgressBar", -1);
		defer.resolve();
	    }
	});
	return defer.promise();
    }
};
