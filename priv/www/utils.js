msrs.Utils = function() {
    this.initialize();
};

msrs.Utils.prototype = {
    initialize: function() {
    },
    xhrPost: function(url, params, callbacks) {
        dojo.publish("changeProgress", [true]);
        var request = {
            url: url,
            handleAs: "json",
            load: dojo.hitch(this, function(response) {
                callbacks.load(response);
                dojo.publish("changeProgress", [false]);
            }),
            error: dojo.hitch(this, function(message) {
                if (callbacks.error) {
                    callbacks.error(message);
                }
                dojo.publish("showErrorMessage", [message]);
                dojo.publish("changeProgress", [false]);
            })
        };
        if (params) {
            request.headers = {
                "Content-Type": "application/x-www-form-urlencoded; charset=utf-8"
            };
            request.postData = dojo.objectToQuery(params);
        }
        dojo.xhrPost(request);
    },
    endsWith: function(source, suffix) {
        var sub = source.length - suffix.length;
        return (sub >= 0) && (source.lastIndexOf(suffix) === sub);
    }
};

msrs.utils = new msrs.Utils();
