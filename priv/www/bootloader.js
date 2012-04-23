var msrs = function() {};

msrs.BootLoader = function() {
    this.initialize();
};

msrs.BootLoader.prototype = {
    categoryPane: null,
    loginPane: null,
    serverPane: null,
    confirmDialog: null,
    registerNewUserDialog: null,
    adminServersDialog: null,
    initialize: function() {
        this.loginPane = new msrs.LoginPane();
        this.categoryPane = new msrs.CategoryPane();
        this.serverPane = new msrs.ServerPane();
        this.confirmDialog = new msrs.ConfirmDialog();
        this.registerNewUserDialog = new msrs.RegisterNewUserDialog();
        this.adminServersDialog = new msrs.AdminServersDialog();
        this.setupEvents();
        dojo.publish("start", []);
    },
    setupEvents: function() {
        dojo.subscribe("changeProgress", this, "onChangeProgress");
        dojo.subscribe("showErrorMessage", this, "showErrorMessage");
        dojo.subscribe("start", this, "onStart");
        dojo.subscribe("selectCategory", this, "onSelectCategory");
        dojo.subscribe("showConfirmDialog", this, "onShowConfirmDialog");
        dojo.subscribe("startRegisterNewUser", this, "onStartRegisterNewUser");
        dojo.subscribe("setUserEmail", this, "onSetUserEmail");
        dojo.subscribe("startAdminServers", this, "onStartAdminServers");
        dojo.connect(dojo.byId("logout"), "click",
                     dojo.hitch(this, "onClickLogout"));
    },
    onChangeProgress: function(state) {
        var progress = dojo.byId("progress");
        if (state) {
            dojo.style(progress, "visibility", "visible");
        } else {
            dojo.style(progress, "visibility", "hidden");
        }
    },
    showErrorMessage: function(message) {
        var e = dojo.byId("errorMessage");
        e.innerHTML = message;
        setTimeout(function() {
            e.innerHTML = "";
        }, 5000);
    },
    onStart: function() {
        msrs.utils.xhrPost("ajax/get_user_info", null, {
            load: dojo.hitch(this, function(response) {
                this.onReceiveGetUserInfo(response);
            })
        });
    },
    onReceiveGetUserInfo: function(response) {
        var result = response.result;
        if (result == "not_found") {
            localStorage["user_id"] = "";
            dojo.style(dojo.byId("userInfo"), "display", "none");
            this.needAuthentication();
        } else {
            localStorage["user_id"] = result.id;
            dojo.style(dojo.byId("userInfo"), "display", "block");
            dojo.byId("userName").innerHTML = "";
            var userNameText = document.createTextNode(result.name);
            dojo.byId("userName").appendChild(userNameText);
            this.serverPane.clear();
            this.categoryPane.getCategories();
        }
    },
    needAuthentication: function() {
        this.categoryPane.clear();
        this.serverPane.clear();
        this.loginPane.show();
    },
    onClickLogout: function(evt) {
        msrs.utils.xhrPost("ajax/logout", null, {
            load: dojo.hitch(this, function(response) {
                dojo.publish("start", []);
            })
        });
    },
    onSelectCategory: function(category) {
        this.serverPane.setCategory(category);
    },
    onShowConfirmDialog: function(message, handler) {
        this.confirmDialog.show(message, handler);
    },
    onStartRegisterNewUser: function() {
        this.registerNewUserDialog.show();
    },
    onSetUserEmail: function(email) {
        this.loginPane.setEmail(email);
    },
    onStartAdminServers: function() {
        this.adminServersDialog.show();
    }
};

dojo.addOnLoad(function() {
    msrs.bootLoader = new msrs.BootLoader();
});