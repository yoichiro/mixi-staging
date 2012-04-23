msrs.CategoryPane = function() {
    this.initialize();
};

msrs.CategoryPane.prototype = {
    initialize: function() {
        this.setupUI();
    },
    setupUI: function() {
        dojo.connect(dojo.byId("adminServers"), "click",
                     dojo.hitch(this, "onClickAdminServers"));
    },
    onClickAdminServers: function() {
        dojo.publish("startAdminServers", []);
    },
    getCategories: function() {
        msrs.utils.xhrPost("ajax/get_categories", null, {
            load: dojo.hitch(this, function(response) {
                this.renderCategories(response.result);
            }),
        });
    },
    renderCategories: function(categories) {
        var self = this;
        var categoriesDiv = dojo.byId("categories");
        categoriesDiv.innerHTML = "";
        dojo.forEach(categories, dojo.hitch(this, function(category) {
            var categoryDiv = document.createElement("div");
            dojo.addClass(categoryDiv, "category");
            var categoryText = document.createTextNode(category.name);
            categoryDiv.appendChild(categoryText);
            categoriesDiv.appendChild(categoryDiv);
            dojo.connect(categoryDiv, "click",
                         dojo.hitch(this, function(category) {
                             return dojo.hitch(self, function(evt) {
                                 this.onClickCategory(category);
                             });
                         }(category)));
        }));
    },
    clear: function() {
        var categoriesDiv = dojo.byId("categories");
        categoriesDiv.innerHTML = "";
    },
    onClickCategory: function(category) {
        dojo.publish("selectCategory", [category]);
    }
};