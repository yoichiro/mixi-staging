msrs.ConfirmDialog = function() {
    this.initialize();
};

msrs.ConfirmDialog.prototype = {
    dialog: null,
    handler: null,
    okButton: null,
    cancelButton: null,
    initialize: function() {
        this.setupUI();
    },
    setupUI: function() {
        this.okButton = new dijit.form.Button({
            label: "O K",
        }, "confirmOkButton");
        dojo.connect(this.okButton, "onClick", dojo.hitch(this, function() {
            this.okButton.set("disabled", true);
            this.dialog.hide();
            this.handler(true);
            this.okButton.set("disabled", false);
        }));
        this.cancelButton = new dijit.form.Button({
            label: "Cancel"
        }, "confirmCancelButton");
        dojo.connect(this.cancelButton, "onClick", dojo.hitch(this, function() {
            this.dialog.hide();
            this.handler(false);
        }));
        this.dialog = new dijit.Dialog({
            closable: false,
            title: "Confirm",
            draggable: false
        }, "confirmDialog");
        dojo.connect(this.dialog, "onHide",
                     dojo.hitch(this, function() {
                         this.handler(false);
                     }));
    },
    show: function(message, callback) {
        var confirmMessage = dojo.byId("confirmMessage");
        confirmMessage.innerHTML = "";
        confirmMessage.appendChild(document.createTextNode(message));
        this.dialog.show();
        this.handler = callback;
    }
};