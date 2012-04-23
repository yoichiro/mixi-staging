msrs.AdminServersDialog = function() {
    this.initialize();
};

msrs.AdminServersDialog.prototype = {
    dialog: null,
    categoriesSelect: null,
    deleteCategoryButton: null,
    categoryTextBox: null,
    addCategoryButton: null,
    serverTextBox: null,
    addServerButton: null,
    initialize: function() {
        this.setupUI();
    },
    setupUI: function() {
        this.categoriesSelect = new dijit.form.Select({
            maxHeight: "200"
        }, "adminCategories");
        dojo.connect(this.categoriesSelect, "onChange", dojo.hitch(this, "onChangeCategories"));
        this.deleteCategoryButton = new dijit.form.Button({
            label: "削除"
        }, "deleteCategoryButton");
        dojo.connect(this.deleteCategoryButton, "onClick",
                     dojo.hitch(this, "onClickDeleteCategoryButton"));
        this.categoryTextBox = new dijit.form.TextBox({
            placeHolder: "カテゴリ名を入力してください。"
        }, "newCategory");
        dojo.style(this.categoryTextBox.domNode, "width", "300px");
        this.addCategoryButton = new dijit.form.Button({
            label: "追加"
        }, "addCategoryButton");
        dojo.connect(this.addCategoryButton, "onClick",
                     dojo.hitch(this, "onClickAddCategoryButton"));
        this.serverTextBox = new dijit.form.TextBox({
            placeHolder: "サーバ名を入力してください。"
        }, "newServer");
        dojo.style(this.serverTextBox.domNode, "width", "300px");
        this.addServerButton = new dijit.form.Button({
            label: "追加"
        }, "addServerButton");
        dojo.connect(this.addServerButton, "onClick",
                     dojo.hitch(this, "onClickAddServerButton"));
        this.dialog = dijit.byId("adminServersDialog");
        dojo.connect(this.dialog, "onHide", dojo.hitch(this, "onHideDialog"));
    },
    show: function() {
        this.getCategories();
        this.dialog.show();
    },
    onHideDialog: function() {
        dojo.publish("start", []);
    },
    getCategories: function() {
        dojo.forEach(this.categoriesSelect.getOptions(), dojo.hitch(this, function(option) {
            this.categoriesSelect.removeOption(option.value);
        }));
        msrs.utils.xhrPost("ajax/get_categories", null, {
            load: dojo.hitch(this, function(response) {
                this.renderCategories(response.result);
            }),
        });
    },
    renderCategories: function(categories) {
        dojo.forEach(categories, dojo.hitch(this, function(category) {
            this.categoriesSelect.addOption({
                label: category.name,
                value: category.id
            });
        }));
    },
    onChangeCategories: function(categoryId) {
        this.getServers(categoryId);
    },
    getServers: function(categoryId) {
        if (categoryId) {
            msrs.utils.xhrPost("ajax/get_servers", {
                category_id: categoryId
            }, {
                load: dojo.hitch(this, function(response) {
                    this.renderServers(response.result);
                }),
            });
        }
    },
    renderServers: function(servers) {
        var self = this;
        var tbody = dojo.byId("adminServersTable");
        tbody.innerHTML = "";
        dojo.forEach(servers, dojo.hitch(this, function(server) {
            var tr = document.createElement("tr");
            var td1 = document.createElement("td");
            td1.appendChild(document.createTextNode(server.name));
            tr.appendChild(td1);

            var td2 = document.createElement("td");
            dojo.addClass(td2, "deleteLinkTd");
            var del = document.createElement("div");
            dojo.addClass(del, "deleteLink");
            del.appendChild(document.createTextNode("削除"));
            td2.appendChild(del);
            tr.appendChild(td2);

            dojo.connect(del, "click",
                         dojo.hitch(this, function(server) {
                             return dojo.hitch(self, function(evt) {
                                 this.onClickDeleteServer(server);
                             });
                         }(server)));

            tbody.appendChild(tr);
        }));
    },
    onClickDeleteServer: function(server) {
        dojo.publish(
            "showConfirmDialog",
            ["このサーバを本当に削除しますか?",
             dojo.hitch(this, function(result) {
                 if (result) {
                     this.deleteServer(server);
                 }
             })]);
    },
    deleteServer: function(server) {
        msrs.utils.xhrPost("ajax/delete_server", {
            server_id: server.id
        }, {
            load: dojo.hitch(this, function(response) {
                var categoryId = this.categoriesSelect.get("value");
                this.getServers(categoryId);
            }),
        });
    },
    onClickAddCategoryButton: function() {
        var categoryName = this.categoryTextBox.get("value");
        if (categoryName) {
            msrs.utils.xhrPost("ajax/create_category", {
                name: categoryName
            }, {
                load: dojo.hitch(this, function(response) {
                    this.categoryTextBox.set("value", "");
                    this.getCategories();
                }),
            });
        }
    },
    onClickDeleteCategoryButton: function() {
        var categoryId = this.categoriesSelect.get("value");
        if (categoryId) {
            dojo.publish(
                "showConfirmDialog",
                ["このカテゴリを本当に削除しますか?",
                 dojo.hitch(this, function(result) {
                     if (result) {
                         this.deleteCategory();
                     }
                 })]);
        }
    },
    deleteCategory: function() {
        var categoryId = this.categoriesSelect.get("value");
        if (categoryId) {
            msrs.utils.xhrPost("ajax/delete_category", {
                category_id: categoryId
            }, {
                load: dojo.hitch(this, function(response) {
                    this.getCategories();
                }),
            });
        }
    },
    onClickAddServerButton: function() {
        var categoryId = this.categoriesSelect.get("value");
        if (categoryId) {
            var serverName = this.serverTextBox.get("value");
            if (serverName) {
                msrs.utils.xhrPost("ajax/create_server", {
                    category_id: categoryId,
                    name: serverName
                }, {
                    load: dojo.hitch(this, function(response) {
                        this.serverTextBox.set("value", "");
                        this.getServers(categoryId);
                    }),
                });
            }
        }
    }
};
