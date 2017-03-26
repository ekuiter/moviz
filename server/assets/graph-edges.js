function GraphEdges() {
    var self = this;
    
    App().server.shouldInvalidateOnDestruction(this);
    self.invalidate();
}

GraphEdges.prototype = {
    update: function() {
	var self = this;
	svgDocument().find("#actors.edge text").each(function() {
	    var actor = $(this).text();
	    var nodes = $(this).siblings("title").text().split("--");
	    new RoleEdge(actor).prepareTooltip(this, nodes);
	});
    },

    invalidate: function() {
	App().server.cache.edges = null;
    }
};

function RoleEdge(obj) {
    if (!(this instanceof RoleEdge))
	return new RoleEdge(obj);
    if (typeof obj === "object")
	$.extend(this, obj);
    else
	this.role1 = { actor: { readableName: obj } };
}

RoleEdge.prototype = {
    imdbName: function(role) {
        return role && role.actor.lastName + ", " + role.actor.firstName;
    },

    profileUrl: function(metadata, role) {
        return metadata && metadata[role - 1] && metadata[role - 1].profileUrl;
    },
    
    getMetadata: function() {
        var actor1 = this.imdbName(this.role1), actor2 = this.imdbName(this.role2);
	var defer1 = App().server.tmdb.search("actors", actor1);
        if (this.role2 && actor1 !== actor2)
            var defer2 = App().server.tmdb.search("actors", actor2);
        else
            var defer2 = instantPromise();
        return $.when(defer1, defer2).then(function(metadata1, metadata2) {
            return [metadata1, metadata2];
        });
    },

    roleSort: function(role, obj1, obj2) {
        var role1Less = !this.role2 || billingScore(this.role1) < billingScore(this.role2);
	if (role == 1)
	    return role1Less ? obj1 : obj2;
	else
	    return role1Less ? obj2 : obj1;
    },

    profileSort: function(metadata, role) {
        return this.roleSort(role, this.profileUrl(metadata, 1), this.profileUrl(metadata, 2));
    },

    roleHtml: function(role, insert, voiceActor) {
        role = this.roleSort(role, this.role1, this.role2);
	var pronoun = this.gender === "male" ? "He" : this.gender === "female" ? "She" : "They";
        var verb = voiceActor ? "dub" : "play" + (this.gender ? "s" : "");
	var text = "<p>" + pronoun + insert + " " + verb + " <b>";
	if (billingScore(role) <= 3)
	    text += "<span class='ui-icon ui-icon-star'></span>";
	text += (role.name || "?") + "<span>";
        text += (voiceActor ? " (" + role.actor.readableName + ")" : "") + "</span></b>";
        return text + " in <b>" + role.movie.title + "</b></p>";
    },

    unavailableHtml: function(invisible) {
        return "<div class='image'><p>" + (invisible ? "" : "No image available.") + "</p></div>";
    },

    profileHtml: function(profile, voiceActor) {
        return profile ? "<img src='" + profile + "'>" :
            voiceActor ? this.unavailableHtml(true) : "";
    },

    edgeHtml: function(metadata, voiceActor) {
	var text = "";
        var profile1 = this.profileSort(metadata, 1);
        var profile2 = this.profileSort(metadata, 2);
        if (voiceActor && this.imdbName(this.role1) === this.imdbName(this.role2)) {
            profile1 = this.profileUrl(metadata, 1);
            profile2 = null;
        }
	if (profile1 || profile2) {
            text += this.profileHtml(profile1, voiceActor);
            if (this.role2)
                text += this.profileHtml(profile2, voiceActor);
        } else if (metadata)
	    text += this.unavailableHtml();
	else {
	    text += "<div class='image'><div class='loading'></div></div>";
            if (voiceActor && this.role2)
                text += "<div class='image'></div>";
        }
	text += this.roleHtml(1, "", voiceActor);
	if (this.role2)
	    text += "<hr>" + this.roleHtml(2, " also", voiceActor);
	return text;
    },

    textHtml: function(actor, voiceActor, nodes, defer) {
	var self = this;
	defer = defer || $.Deferred();
	if (nodes && !App().server.cache.edges) {
	    defer.notify("<div class='loading'></div>");
	    App().server.getEdges().then(function() {
		return self.textHtml(actor, voiceActor, nodes, defer);
	    });
	} else {
	    if (!nodes)
		var edge = this;
	    else
		var edge = App().server.cache.edges.find(function(edge) {
		    return edge.node1.title === nodes[0] && edge.node2.title === nodes[1]
			&& voiceActor ?
                            edge.voiceActor && edge.voiceActor.readableName === voiceActor :
                            !edge.voiceActor && edge.role1.actor.readableName === actor;
		});
	    if (!edge)
		return "Something went wrong. Try restarting moviz.";

	    defer.notify(edge.edgeHtml(null, voiceActor));
	    edge.getMetadata().then(function(metadata) {
		defer.resolve(edge.edgeHtml(metadata, voiceActor));
	    });
	}
	return defer.promise();
    },

    prepareTooltip: function(elem, nodes, showOnReady) {
	var self = this;
	var actor = this.role1.actor.readableName;
        var voiceActor = this.voiceActor && this.voiceActor.readableName ||
            actor.indexOf(" (VA)") !== -1 && actor.replace(" (VA)", "");

        if (!showOnReady) {
            $(elem).hide().
                html(voiceActor ? voiceActor + " <tspan class='small'>🎤</tspan>" : actor);
            defer($(elem).show.bind($(elem)));
        }
        
	$(elem).qtip({
	    content: {
		title: function(event, api) {
		    return voiceActor || actor;
		},
		text: function(event, api) {
		    return self.textHtml(actor, voiceActor, nodes);
		}
	    },
	    show: { solo: true, ready: showOnReady },
	    style: { classes: "qtip-dark qtip-shadow tooltip edge-tooltip " +
                     (voiceActor ? "voice-actor" + (showOnReady ? " details" : "") : "") },
	    position: { my: "top left", viewport: true },
	    events: {
		show: $(elem).parents("svg").length ?
		    App().graph.adjustPositionFn($(elem), 3, 4) :
		    null
	    }
	});
    }
};
