function ElmBridge() {
    this.apps = {};
}

ElmBridge.prototype = {
    embed: function(elmPackage, elem, flags) {
        if (!window.Elm)
            throw "Elm not found";
        if (!Elm[elmPackage])
            throw "Elm package " + elmPackage + " not found";
        if ($(elem).length === 0)
            throw "Elm target not found";
        this.apps[elmPackage] = this.apps[elmPackage] || [];
        var app = Elm[elmPackage].embed($(elem)[0], flags);
        app.ports.reportError.subscribe(App().reportError.bind(App()));
        this.apps[elmPackage].push(app);
        return app;
    },

    addVoiceActors: function(elem, titles) {
        var app = this.embed("AddVoiceActors", elem, { titles: titles });
        app.getResults = function() {
            var defer = $.Deferred();
            app.ports.selectedEntries.subscribe(function subscription(data) {
                defer.resolve(data);
                app.ports.selectedEntries.unsubscribe(subscription);
            });
            app.ports.results.send(null);
            return defer.promise();
        };
        app.setupProgress = function() {
            defer(function() {
                $(elem).find(".progress").progressbar({ value: false });
            }, 50);
        };
        app.onReady = function() {
            var defer = $.Deferred();
            app.ports.ready.subscribe(defer.resolve.bind(defer));
            return defer.promise();
        };
        app.setState = function(title, state) {
            app.ports.dubbedMovieSelectState.send({ title: title, state: state });
            app.setupProgress();
        };
        app.setupProgress();
        return app;
    }
};
