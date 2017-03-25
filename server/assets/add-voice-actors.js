function AddVoiceActors() {
    var self = this;
    self.dialog = $("#add-voice-actors-dialog");
    makeDialog(self.dialog, { buttons: { "Add": addVoiceActors } });
    self.button = self.dialog.siblings(".ui-dialog-buttonpane").find("button");

    function addVoiceActors() {
        self.app.getResults().then(function(dubbedMovies) {
            self.button.button("disable");
            $.when.apply($, dubbedMovies.map(function(dubbedMovie) {
                self.app.setState(dubbedMovie.title, "loading");
                return App().server.synchronkartei.add(dubbedMovie.title, dubbedMovie.path).
                    then(function() {
                        self.app.setState(dubbedMovie.title, "done");
                    });
            })).then(function() {
                self.dialog.dialog("close");
            });
        });
    }
}

AddVoiceActors.prototype = {
    update: function() {
        var self = this;
        var movies = App().server.cache.nodes.filter(function(node) {
            return node.voiceActors === 0;
        }).map(function(node) { return node.title; });
        if (movies.length === 0)
            return;

        self.button.button("disable");
        self.dialog.dialog("open");
        self.app = App().elmBridge.addVoiceActors(self.dialog.find(".elm"), movies);
        self.app.onReady().then(function() {
            self.button.button("enable");
        });
    }
};
