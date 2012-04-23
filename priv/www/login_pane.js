msrs.LoginPane = function() {
    this.initialize();
};

msrs.LoginPane.prototype = {
    emailTextBox: null,
    passwordTextBox: null,
    loginButton: null,
    loginDialog: null,
    initialize: function() {
        this.setupUI();
    },
    setupUI: function() {
        this.emailTextBox = new dijit.form.TextBox({
            name: "email",
            value: ""
        }, "email");
        dojo.connect(this.emailTextBox, "onKeyPress",
                     dojo.hitch(this, "onPressEnterKey"));
        this.passwordTextBox = new dijit.form.TextBox({
            name: "password",
            value: "",
            type: "password"
        }, "password");
        dojo.connect(this.passwordTextBox, "onKeyPress",
                     dojo.hitch(this, "onPressEnterKey"));
        this.loginButton = new dijit.form.Button({
            label: "Login"
        }, "loginButton");
        dojo.connect(this.loginButton, "onClick",
                     dojo.hitch(this, "onClickLoginButton"));
        this.loginDialog = new dijit.Dialog({
            closable: false,
            title: "Welcome!",
            draggable: false
        }, "loginDialog");
        dojo.connect(this.loginDialog, "onHide",
                     dojo.hitch(this, "onHideLoginDialog"));
        dojo.connect(dojo.byId("registerNewUser"), "click",
                     dojo.hitch(this, "onClickRegisterNewUser"));
    },
    onPressEnterKey: function(evt) {
        if (evt.keyCode == dojo.keys.ENTER) {
            this.onClickLoginButton();
        }
    },
    show: function() {
        this.loginDialog.show();
        var email = localStorage["user_email"];
        if (email) {
            this.emailTextBox.set("value", email);
        }
        this.passwordTextBox.set("value", "");
    },
    onClickRegisterNewUser: function() {
        dojo.publish("startRegisterNewUser", []);
    },
    onHideLoginDialog: function() {
        dojo.byId("loginErrorMessage").innerHTML = "";
        dojo.publish("start", []);
    },
    onClickLoginButton: function(evt) {
        this.authenticate();
    },
    authenticate: function() {
        this.loginButton.set("disabled", true);
        dojo.publish("changeProgress", [true]);
        dojo.byId("loginErrorMessage").innerHTML = "";
        var email = this.emailTextBox.get("value");
        var password = this.passwordTextBox.get("value");
        msrs.utils.xhrPost(
            "ajax/authenticate",
            {email: email, password: password},
            {
                load: dojo.hitch(this, function(response) {
                    this.onReceiveAuthenticate(response);
                    this.loginButton.set("disabled", false);
                }),
                error: dojo.hitch(this, function(message) {
                    this.loginButton.set("disabled", false);
                })
            }
        );
    },
    onReceiveAuthenticate: function(response) {
        var result = response.result;
        if (result) {
            localStorage["user_email"] = this.emailTextBox.get("value");
            this.loginDialog.hide();
            dojo.publish("start", []);
        } else {
            dojo.byId("loginErrorMessage").innerHTML = "認証に失敗しました。";
            dijit.byId("email").focus();
        }
    },
    setEmail: function(email) {
        this.emailTextBox.set("value", email);
    }
};
