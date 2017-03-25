function ElmBridge() {
    this.apps = {};
}

ElmBridge.prototype = {
    embed: function(elmPackage, elem, flags) {
        if (!window.Elm)
            App().reportError("Elm not found");
        if (!Elm[elmPackage])
            App().reportError("Elm package " + elmPackage + " not found");
        this.apps[elmPackage] = this[elmPackage] || [];
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
        return app;
    }
};
