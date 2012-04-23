msrs.RegisterNewUserDialog = function() {
    this.initialize();
};

msrs.RegisterNewUserDialog.prototype = {
    emailTextBox: null,
    password1TextBox: null,
    password2TextBox: null,
    nameTextBox: null,
    ircTextBox: null,
    registerNewUserButton: null,
    dialog: null,
    initialize: function() {
        this.setupUI();
    },
    setupUI: function() {
        this.emailTextBox = new dijit.form.ValidationTextBox({
            placeHolder: "メールアドレス",
            required: true
        }, "newEmail");
        this.password1TextBox = new dijit.form.ValidationTextBox({
            placeHolder: "パスワード",
            type: "password",
            required: true
        }, "newPassword1");
        this.password2TextBox = new dijit.form.ValidationTextBox({
            placeHolder: "パスワードを再入力",
            type: "password",
            required: true
        }, "newPassword2");
        this.nameTextBox = new dijit.form.ValidationTextBox({
            placeHolder: "お名前",
            required: true
        }, "newName");
        this.ircTextBox = new dijit.form.ValidationTextBox({
            placeHolder: "IRCのニックネーム",
            required: true
        }, "newIrc");
        this.registerNewUserButton = new dijit.form.Button({
            label: "登録",
            onClick: dojo.hitch(this, function() {
                this.onClickRegisterNewUserButton();
            })
        }, "registerNewUserButton");
        this.dialog = new dijit.Dialog({
            title: "新規ユーザ登録",
            draggable: false
        }, "registerNewUserDialog");
    },
    show: function() {
        dojo.byId("registerNewUserErrorMessage").innerHTML = "";
        this.emailTextBox.set("value", "");
        this.password1TextBox.set("value", "");
        this.password2TextBox.set("value", "");
        this.nameTextBox.set("value", "");
        this.ircTextBox.set("value", "");
        this.dialog.show();
    },
    onClickRegisterNewUserButton: function() {
        this.registerNewUserButton.set("disabled", true);
        if (this.validateForm()) {
            dojo.byId("registerNewUserErrorMessage").innerHTML = "";
            var email = this.emailTextBox.get("value");
            var password = this.password1TextBox.get("value");
            var name = this.nameTextBox.get("value");
            var irc = this.ircTextBox.get("value");
            msrs.utils.xhrPost("ajax/register_new_user", {
                email: email,
                password: password,
                name: name,
                irc: irc
            }, {
                load: dojo.hitch(this, function(response) {
                    this.onReceiveRegisterNewUser(response);
                    this.registerNewUserButton.set("disabled", false);
                })
            });
        } else {
            this.registerNewUserButton.set("disabled", false);
        }
    },
    validateForm: function() {
        this.emailTextBox.validate();
        this.password1TextBox.validate();
        this.password2TextBox.validate();
        this.nameTextBox.validate();
        this.ircTextBox.validate();
        var result = this.emailTextBox.isValid()
            && this.password1TextBox.isValid()
            && this.password2TextBox.isValid()
            && this.nameTextBox.isValid()
            && this.ircTextBox.isValid();
        if (result) {
            if (!dojox.validate.isEmailAddress(
                this.emailTextBox.get("value"))) {
                dojo.byId("registerNewUserErrorMessage").innerHTML =
                    "Emailの入力に不備があります。";
                return false;
            }
            if (this.password1TextBox.get("value")
                != this.password2TextBox.get("value")) {
                dojo.byId("registerNewUserErrorMessage").innerHTML =
                    "パスワードが一致しません。";
                return false;
            }
            return true;
        } else {
            dojo.byId("registerNewUserErrorMessage").innerHTML =
                "入力に不備があります。";
            return false;
        }
    },
    onReceiveRegisterNewUser: function(response) {
        if (response.result) {
            var email = this.emailTextBox.get("value");
            dojo.publish("setUserEmail", [email]);
            this.dialog.hide();
        } else {
            dojo.byId("registerNewUserErrorMessage").innerHTML =
                "同じEmailのユーザが既にいます。";
        }
    }
};