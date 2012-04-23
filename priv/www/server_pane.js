msrs.ServerPane = function() {
    this.initialize();
};

msrs.ServerPane.prototype = {
    fromDateTextBox: null,
    toDateTextBox: null,
    branchTextBox: null,
    purposeTextBox: null,
    reserveButton: null,
    selectedCategory: null,
    initialize: function() {
        this.setupUI();
    },
    setupUI: function() {
        this.fromDateTextBox = new dijit.form.DateTextBox({
            required: true,
            value: new Date()
        }, "fromDate");
        this.fromDateTextBox.constraints.max = this.computeMonth(new Date(), 12);
        this.fromDateTextBox.constraints.min = new Date();
        dojo.connect(this.fromDateTextBox, "onChange",
                     dojo.hitch(this, function(evt) {
                         this.toDateTextBox.constraints.min = evt;
                     }));
        this.toDateTextBox = new dijit.form.DateTextBox({
            required: true,
            value: new Date()
        }, "toDate");
        this.toDateTextBox.constraints.max = this.computeMonth(new Date(), 12);
        this.toDateTextBox.constraints.min = new Date();
        dojo.connect(this.toDateTextBox, "onChange",
                     dojo.hitch(this, function(evt) {
                         this.fromDateTextBox.constraints.max = evt;
                     }));
        this.branchTextBox = new dijit.form.ValidationTextBox({
            placeHolder: "パスを入力してください。",
            required: true,
            invalidMessage: "必須入力です。"
        }, "branch");
        dojo.style(this.branchTextBox.domNode, "width", "400px");
        this.purposeTextBox = new dijit.form.ValidationTextBox({
            placeHolder: "プロジェクト名などを入力してください。",
            required: true,
            invalidMessage: "必須入力です。"
        }, "purpose");
        dojo.style(this.purposeTextBox.domNode, "width", "400px");
        this.reserveButton = new dijit.form.Button({
            label: "予約する",
            onClick: dojo.hitch(this, function() {
                this.onClickReserveButton();
            })
        }, "reserveButton");
    },
    clear: function() {
        dojo.style(dojo.byId("serverPane"), "display", "none");
    },
    onClickReserveButton: function() {
        if (!this.validateReserveForm()) {
            return;
        }
        this.reserveButton.set("disabled", true);
        var fromDateSource = this.fromDateTextBox.get("value");
        var fromDate = this.formatDate(fromDateSource);
        var toDateSource = this.toDateTextBox.get("value");
        var toDate = this.formatDate(toDateSource);
        var branch = this.branchTextBox.get("value");
        var purpose = this.purposeTextBox.get("value");
        msrs.utils.xhrPost("ajax/reserve", {
                from: fromDate,
                to: toDate,
                branch: branch,
                category_id: this.selectedCategory.id,
                purpose: purpose
        }, {
            load: dojo.hitch(this, function(response) {
                this.onReceiveReserve(response.result);
                this.reserveButton.set("disabled", false);
            }),
            error: dojo.hitch(this, function(message) {
                this.reserveButton.set("disabled", false);
            })
        });
    },
    validateReserveForm: function() {
        this.fromDateTextBox.validate();
        this.toDateTextBox.validate();
        this.branchTextBox.validate();
        this.purposeTextBox.validate();
        var formValidated = this.fromDateTextBox.isValid()
            && this.toDateTextBox.isValid()
            && this.branchTextBox.isValid()
            && this.purposeTextBox.isValid();
        if (formValidated) {
            var from = this.formatDate(this.fromDateTextBox.get("value"));
            var to = this.formatDate(this.toDateTextBox.get("value"));
            if (from <= to) {
                return true;
            } else {
                this.showAlertDialog("不正な期間が指定されています。");
                return false;
            }
        } else {
            this.showAlertDialog("入力に不備があります。");
            return false;
        }
    },
    showAlertDialog: function(message) {
        var dialog = new dijit.Dialog({
            title: "Warning",
            content: message,
            draggable: false
        });
        dialog.show();
    },
    onReceiveReserve: function(reserved) {
        if (reserved) {
            this.getBookings();
        } else {
            this.showAlertDialog("その期間で予約可能なサーバがありません。");
        }
    },
    formatDate: function(source) {
        var fmt = "yyyyMMdd";
        return Number(
            dojo.date.locale.format(
                source, {selector: "date", datePattern: fmt}));
    },
    setCategory: function(category) {
        this.selectedCategory = category;
        var categoryNameDiv = dojo.byId("categoryName");
        categoryNameDiv.innerHTML = "";
        var categoryNameText = document.createTextNode(category.name);
        categoryNameDiv.appendChild(categoryNameText);
        this.getBookings();
        dojo.style(dojo.byId("serverPane"), "display", "block");
    },
    getBookings: function() {
        msrs.utils.xhrPost("ajax/get_bookings", {
                category_id: this.selectedCategory.id
        }, {
            load: dojo.hitch(this, function(response) {
                this.onReceiveBookings(response.result);
            })
        });
    },
    onReceiveBookings: function(bookings) {
        var self = this;
        var bookingsBody = dojo.byId("bookings");
        bookingsBody.innerHTML = "";
        if (bookings.length > 0) {
            dojo.style(dojo.byId("bookingsTable"), "display", "block");
            dojo.style(dojo.byId("no_any_bookings"), "display", "none");
            dojo.forEach(bookings, dojo.hitch(this, function(booking) {
                var tr = document.createElement("tr");

                var server = document.createElement("a");
                var url = "http://";
                if (msrs.utils.endsWith(booking.server_name, ".jp")) {
                    url += booking.server_name + "/";
                } else {
                    url += booking.server_name + ".st.mixi.jp/";
                }
                server.setAttribute("href", url);
                server.setAttribute("target", "_blank");
                server.id = booking.server_name;
                server.appendChild(document.createTextNode(booking.server_name));
                var serverTd = document.createElement("td");
                serverTd.appendChild(server);
                tr.appendChild(serverTd);

                this.appendTdNode(tr, booking.branch);
                this.appendTdNode(tr, this.createDateString(booking.from));
                this.appendTdNode(tr, this.createDateString(booking.to));

                var user = document.createElement("a");
                user.setAttribute("href", "mailto:" + booking.user_email);
                user.setAttribute("title", "IRC: " + booking.user_irc);
                user.appendChild(document.createTextNode(booking.user_name));
                var userTd = document.createElement("td");
                userTd.appendChild(user);
                tr.appendChild(userTd);

                this.appendTdNode(tr, booking.purpose);

                var delTd = document.createElement("td");
                dojo.addClass(delTd, "deleteLinkTd");
                if (localStorage["user_id"] == booking.user_id) {
                    var delDiv = document.createElement("div");
                    dojo.addClass(delDiv, "deleteLink");
                    var delText = document.createTextNode("取り消し");
                    delDiv.appendChild(delText);
                    delTd.appendChild(delDiv);

                    dojo.connect(delDiv, "click",
                                 dojo.hitch(this, function(booking) {
                                     return dojo.hitch(self, function(evt) {
                                         this.onClickDeleteBooking(booking);
                                     });
                                 }(booking)));
                }
                tr.appendChild(delTd);

                bookingsBody.appendChild(tr);

                dijit.Tooltip.defaultPosition=["below"];
                new dijit.Tooltip({
                    connectId: [booking.server_name],
                    label: "<img src='qrcode?url=" + url + "' width='150' height='150' />"
                });
            }));
        } else {
            dojo.style(dojo.byId("bookingsTable"), "display", "none");
            dojo.style(dojo.byId("no_any_bookings"), "display", "block");
        }
    },
    appendTdNode: function(parent, body) {
        var td = document.createElement("td");
        var text = document.createTextNode(body);
        td.appendChild(text);
        parent.appendChild(td);
    },
    createDateString: function(source) {
        var str = String(source);
        return str.substring(0, 4) + "-" + str.substring(4, 6) + "-" + str.substring(6,8);
    },
    onClickDeleteBooking: function(booking) {
        dojo.publish(
            "showConfirmDialog",
            ["この予約を取り消しますか?",
             dojo.hitch(this, function(result) {
                 if (result) {
                     this.deleteBooking(booking);
                 }
             })]);
    },
    deleteBooking: function(booking) {
        msrs.utils.xhrPost("ajax/delete_booking", {
                booking_id: booking.id
        }, {
            load: dojo.hitch(this, function(response) {
                this.onReceiveDeleteBooking(response.result);
            })
        });
    },
    onReceiveDeleteBooking: function(result) {
        this.getBookings();
    },
    computeMonth: function(source, addMonths) {
        var year = source.getFullYear();
        var month = source.getMonth();
        var day = source.getDate();
        month += addMonths;
        var endDay = this.getMonthEndDay(year, month);
        if (day > endDay) {
            day = endDay;
        }
        var dt = new Date(year, month - 1, day);
        return dt;
    },
    getMonthEndDay: function(year, month) {
        var dt = new Date(year, month, 0);
        return dt.getDate();
    }
};