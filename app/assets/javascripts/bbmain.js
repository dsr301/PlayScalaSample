(function() {
    window.NlApp = {
        Models: {},
        Collections: {},
        Views: {},
        Router: {}
    };
    NlApp.Router = Backbone.Router.extend({
        initialize: function() {
            this.getLocation();
        },
        routes: {
            '': 'index',
            'business/show/:id': 'loadBusiness',
            'business': 'loadFullData',
            'business/:bId': 'loadData',
            'posts': 'loadPosts',
            'events': 'loadEvents',
            'events/nextpage/:id': 'loadEventsPage',
            'addEvents': 'createEvents',
            //'savePosts': 'savePosts',
            'postComment/:id': 'postComment',
            'postsDiscuss': 'loadPostsDisqus',
            'business/:bId/nextpage/:id': 'loadNextPage',
            'addBusiness': 'createBusiness',
            'editBusiness/:bId': 'editBusiness',
            'updateBusiness/:bId': 'updateBusiness',
            'rateBusiness/:bId': 'rateBusiness',
            'saveBusiness': 'saveBusiness',
            'uploadForm/:bId': 'uploadForm',
            'uploadPhotos/:bId': 'uploadPhotos'
        },
        getLocation: function() {
            console.log('comes first here');
            $.getScript("http://j.maxmind.com/app/geoip.js", function(data, textStatus, jqxhr) {
                console.log(geoip_city());
                var city = geoip_city();
                var navView = new NlApp.Views.NavView({
                    'currentCity': city
                });
            });
        },
        index: function() {
            console.log('hi there from the index page');
        },
        loadBusiness: function(id) {
            $('#main').empty();
            if (id) {
                new NlApp.Views.BusinessView({
                    id: id
                });
            }
        },
        loadNextPage: function(bId, id) {
            var c = $("#cd").val();
            var r = $("#rd").val();
            var f = $("#mainNavText").val();
            if (!f || f.length < 1) {
                f = "";
            }
            new NlApp.Views.ListsView({
                c: c,
                r: r,
                f: f,
                b: bId,
                p: id
            });
        },
        loadData: function(bId) {
            var c = $("#cd").val();
            var r = $("#rd").val();
            var f = $("#mainNavText").val();
            if (!f || f.length < 1) {
                f = "";
            }
            new NlApp.Views.ListsView({
                c: c,
                r: r,
                f: f,
                b: bId
            });
        },
        loadFullData: function() {
            var c = $("#cd").val();
            var r = $("#rd").val();
            var f = $("#mainNavText").val();
            if (!f || f.length < 1) {
                f = "";
            }
            new NlApp.Views.ListsView({
                c: c,
                r: r,
                f: f
            });
        },
        loadPosts: function() {
            var c = $("#cd").val();
            var r = $("#rd").val();

            new NlApp.Views.DiscussionView({
                c: c,
                r: r
            });
            /*var c = $("#cd").val();
            var r = $("#rd").val();
            $("#main").load("/posts?r=" + r + "&c=" + c);*/
        },
        loadEvents: function() {
            var c = $("#cd").val();
            var r = $("#rd").val();
            $("#main").load("/events?r=" + r + "&c=" + c);
        },
        loadEventsPage: function(p) {
            var c = $("#cd").val();
            var r = $("#rd").val();
            $("#main").load("/events?r=" + r + "&c=" + c+"&p="+p);
        },
        postComment: function(id) {
            var formData = new FormData($("#commentForm")[0]);
            var url1 = '/posts/' + id + '/comments';
            $.ajax({
                url: url1, //Server script to process data
                type: 'POST',
                success: function(data) {
                    $("#main").html(data);
                }, // Form data
                data: formData, //Options to tell jQuery not to process data or worry about content-type.
                cache: false,
                contentType: false,
                processData: false
            });

        },
        loadPostsDisqus: function() {
            $("#main").load("/postsDisqus");
        },
        createBusiness: function() {
            new NlApp.Views.AddBusinessView();
        },
        createEvents: function() {
            new NlApp.Views.AddEventView();
        },
        editBusiness: function(id) {
            $('#main').empty();
            if (id) {
                $("#main").load("/business/" + id);
            }
        },
        updateBusiness: function(id) {
            var formData = new FormData($("#updateForm")[0]);
            var c = $("#cd").val();
            var r = $("#rd").val();
            var url1 = '/business/' + id;
            $.ajax({
                url: url1, //Server script to process data
                type: 'POST',
                success: function(data) {
                    $("#main").html(data);
                }, // Form data
                data: formData, //Options to tell jQuery not to process data or worry about content-type.
                cache: false,
                contentType: false,
                processData: false
            });
        },
        rateBusiness: function(bId) {
            $("#main").load("/business/rate/" + bId);
        },
        saveBusiness: function() {
            var formData = new FormData($("#ratingForm")[0]);
            var url1 = '/business/rate';
            $.ajax({
                url: url1, //Server script to process data
                type: 'POST',
                success: function(data) {
                    $("#main").html(data);
                }, // Form data
                data: formData, //Options to tell jQuery not to process data or worry about content-type.
                cache: false,
                contentType: false,
                processData: false
            });
        },
        uploadForm: function(bId) {
            $("#main").load("/uploadForm/" + bId);
        },
        uploadPhotos: function(bId) {
            var formData = new FormData($("#photosForm")[0]);
            var url1 = '/uploadPhotos/' + bId;
            $.ajax({
                url: url1, //Server script to process data
                type: 'POST',
                success: function(data) {
                    $("#main").html(data);
                }, // Form data
                data: formData, //Options to tell jQuery not to process data or worry about content-type.
                cache: false,
                contentType: false,
                processData: false
            });
        }
    });

    NlApp.Views.NavView = Backbone.View.extend({
        el: '#mainNav',
        currentCity: '',
        events: {
            'click #cityMenu': 'cityChange',
            'change #rd': 'regionChange',
            'keypress #mainNavText': 'searchClick'
        },
        city: {
            'Hyderabad': 1,
            'Bangalore': 2,
            'Delhi': 3,
            'Chennai':4,
            'Mumbai': 5
        },
        cityChange: function(event) {
            console.log(event);
            console.log($(this));
            var key, $elem;
            if(event) {
                event.preventDefault();
                $elem = $(event.target);
                key = this.city[$elem.text()];
                $('#cd').val(key);
                $('#cityDDM').html($elem.text()+ ' <b class="caret"></b>');
            } else {
                key = this.city[$('#cityDDM').text().trim()];
                $('#cd').val(key);
            }
            
            if (key > 0) {

            } else {
                $("#rd").empty();
                $("#rd").append('<option value=0>Please choose City</option>');
                return;
            }

            jsRoutes.controllers.Application.getRegion(key).ajax({
                context: this,
                success: function(data) {
                    $("#rd").empty();
                    $("#rd").append('<option value=0>Select Region</option>');
                    $select = $('#rd');
                    // remove any exisiting contents

                    //iterate over the data and append a select option for each item
                    $.each(data, function(key, val) {
                        $select.append('<option value="' + val.id + '">' + val.name + '</option>');
                    });

                    // enable the select control
                    $select.prop('disabled', false);
                },
                error: function() {
                    console.debug("Error of ajax Call");
                    console.debug(err);
                }
            });
            
            jsRoutes.controllers.Application.getLastRegId().ajax({
                context: this,
                success: function(data) {
                    $select = $('#rd');
                    // remove any exisiting contents
                    if(!(data==="none")){
                        $select.val(Number(data));
                    }

                    
                },
                error: function() {
                    console.debug("Error of ajax Call");
                    console.debug(err);
                }
            });
        },
        regionChange: function(event) {
            var c = $("#cd").val();
            var r = $("#rd").val();
            new NlApp.Views.ListsView({
                c: c,
                r: r
            });
        },
        searchClick: function(event) {
            var key = window.event.keyCode;

            // If the user has pressed enter
            if (key == 13) {
                this.filterData();
            }
        },
        filterData: function() {
            var c = $("#cd").val();
            var r = $("#rd").val();
            var f = $("#mainNavText").val();
            new NlApp.Views.ListsView({
                c: c,
                r: r,
                f: f
            });
        },
        initialize : function() {
            this.render();
        },
        render: function() {
            var currentCity = this.options.currentCity;
            if(currentCity && this.city[currentCity]) {
                $('#cityDDM').html(currentCity + ' <b class="caret"></b>');
                this.cityChange();
            }
            return this;
        }
    });

    NlApp.Views.ListsView = Backbone.View.extend({
        el: '#main',
        c: '',
        r: '',
        b: 0,
        f: '',
        p: 0,
        initialize: function() {
            this.render();
        },
        events: {
            'keypress #mainNavText': 'filterData',
            'click .lookImages': 'showModal',
            'click .lookImage': 'imageClick'
        },
        render: function() {
            var c = this.options.c || this.c;
            var r = this.options.r || this.r;
            var f = this.options.f || this.f;
            var b = this.options.b || this.b;
            var p = this.options.p || this.p;
            var that = this; //**correct this..memory leak
            $.get("/business?p=" + p + "&r=" + r + "&c=" + c + "&b=" + b + "&f=" + f, function(template) {
                that.$el.html(template);
                return that;
            });
            return this;
        },
        filterData: function(event) {
            var key = window.event.keyCode;

            // If the user has pressed enter
            if (key == 13) {
                var c = $("#cd").val();
                var r = $("#rd").val();
                var f = $("#mainNavText").val();
                new NlApp.Views.ListsView({
                    c: c,
                    r: r,
                    f: f
                });
            }
        },
        showModal: function(event) {
            event.preventDefault();
            console.log(event);
            var imgLinks = $(event.target).closest('.lookImages').find('.lookImage').map(function(i, el) {
                return $(el).attr('href');
            });
            var fancyArr = [];
            $.each(imgLinks, function(index, value) {
                fancyArr.push({
                    href: value,
                    title: 'Test' + index
                });
            });
            $.fancybox.open(fancyArr, {
                padding: 0
            });
            /*$.fancybox.open([{
                href: '/assets/images/17DegreeNorth/337l.png',
                title: '1st title'
            }, {
                href: '/assets/images/17DegreeNorth/337l.png',
                title: '2nd title'
            }, {
                href: '/assets/images/17DegreeNorth/337l.png',
                title: '3rd title'
            }], {
                padding: 0
            });*/
        },
        imageClick: function(event) {
            event.preventDefault();
        }
    });

    NlApp.Views.AddBusinessView = Backbone.View.extend({
        el: '#main',
        events: {
            'change #cx': 'cityChange',
            'click :button': 'submitForm'
        },
        initialize: function() {
            this.render();
        },
        render: function() {
            var that = this; //**correct this..memory leak
            $.get("/business/new", function(template) {
                //var html = $(template);
                that.$el.html(template);
                return that;
            });
            return this;
        },
        cityChange: function(event) {
            var $dropdown = $(event.target);
            var key = $dropdown.val();

            if (key > 0) {

            } else {
                $("#rx").empty();
                $("#rx").append('<option value=0>Please choose City</option>');
                return;
            }

            jsRoutes.controllers.Application.getRegion(key).ajax({
                context: this,
                success: function(data) {
                    $("#rx").empty();
                    $("#rx").append('<option value=0>--Select Region--</option>');
                    $select = $('#rx');
                    // remove any exisiting contents

                    //iterate over the data and append a select option for each item
                    $.each(data, function(key, val) {
                        $select.append('<option value="' + val.id + '">' + val.name + '</option>');
                    });

                    // enable the select control
                    $select.prop('disabled', false);
                },
                error: function() {
                    console.debug("Error of ajax Call");
                    console.debug(err);
                }
            });
        },
        submitForm: function(event) {
            var formData = new FormData($('form')[0]);
            $.ajax({
                url: 'businessSave', //Server script to process data
                type: 'POST',
                context: this,
                xhr: function() { // Custom XMLHttpRequest
                    var myXhr = $.ajaxSettings.xhr();
                    if (myXhr.upload) { // Check if upload property exists
                        myXhr.upload.addEventListener('progress', this.progressHandlingFunction, false); // For handling the progress of the upload
                    }
                    return myXhr;
                },
                //Ajax events
                success: function(data) {
                    this.$el.html(data);
                },
                // Form data
                data: formData,
                //Options to tell jQuery not to process data or worry about content-type.
                cache: false,
                contentType: false,
                processData: false
            });
        },
        progressHandlingFunction: function(event) {
            if (event.lengthComputable) {
                $('progress').attr({
                    value: event.loaded,
                    max: event.total
                });
            }
        }

    });


    NlApp.Views.AddEventView = Backbone.View.extend({
        el: '#main',
        events: {
            'change #cx': 'cityChange',
            'click :button': 'submitForm'
        },
        initialize: function() {
            this.render();
        },
        render: function() {
            var that = this; //**correct this..memory leak
            $.get("/event/new", function(template) {
                //var html = $(template);
                that.$el.html(template);
                return that;
            });
            return this;
        },
        cityChange: function(event) {
            var $dropdown = $(event.target);
            var key = $dropdown.val();

            if (key > 0) {

            } else {
                $("#rx").empty();
                $("#rx").append('<option value=0>Please choose City</option>');
                return;
            }

            jsRoutes.controllers.Application.getRegion(key).ajax({
                context: this,
                success: function(data) {
                    $("#rx").empty();
                    $("#rx").append('<option value=0>--Select Region--</option>');
                    $select = $('#rx');
                    // remove any exisiting contents

                    //iterate over the data and append a select option for each item
                    $.each(data, function(key, val) {
                        $select.append('<option value="' + val.id + '">' + val.name + '</option>');
                    });

                    // enable the select control
                    $select.prop('disabled', false);
                },
                error: function() {
                    console.debug("Error of ajax Call");
                    console.debug(err);
                }
            });
        },
        submitForm: function(event) {
            var formData = new FormData($('form')[0]);
            var url = 'eventSave/' + $("#cx").val() + '/' + $("#rx").val();
            $.ajax({
                url: url, //Server script to process data
                type: 'POST',
                context: this,
                xhr: function() { // Custom XMLHttpRequest
                    var myXhr = $.ajaxSettings.xhr();
                    if (myXhr.upload) { // Check if upload property exists
                        myXhr.upload.addEventListener('progress', this.progressHandlingFunction, false); // For handling the progress of the upload
                    }
                    return myXhr;
                },
                //Ajax events
                success: function(data) {
                    this.$el.html(data);
                },
                // Form data
                data: formData,
                //Options to tell jQuery not to process data or worry about content-type.
                cache: false,
                contentType: false,
                processData: false
            });
        },
        progressHandlingFunction: function(event) {
            if (event.lengthComputable) {
                $('progress').attr({
                    value: event.loaded,
                    max: event.total
                });
            }
        }

    });



    NlApp.Views.BusinessView = Backbone.View.extend({
        el: '#main',
        id: '',
        events: {
            'click .lookImages': 'showModal',
            'click .lookImage': 'imageClick'
        },
        initialize: function() {
            this.render();
        },
        render: function() {
            var id = this.options.id || this.id;
            var that = this; //**correct this..memory leak
            $.get("/business/show/" + id, function(template) {
                that.$el.html(template);
                return that;
            });

            return this;
        },
        showModal: function(event) {
            event.preventDefault();
            console.log(event);
            var imgLinks = $(event.target).closest('.lookImages').find('.lookImage').map(function(i, el) {
                return $(el).attr('href');
            });
            var fancyArr = [];
            $.each(imgLinks, function(index, value) {
                fancyArr.push({
                    href: value,
                    title: 'Test' + index
                });
            });
            $.fancybox.open(fancyArr, {
                padding: 0
            });
        },
        imageClick: function(event) {
            event.preventDefault();
        }
    });

    NlApp.Views.DiscussionView = Backbone.View.extend({
        el: '#main',
        id: '',
        events: {
            'click #nearlook_fake': 'showForm',
            'click #close_button': 'closePost',
            'click #post_button': 'savePosts'
        },
        initialize: function() {
            this.render();
        },
        render: function() {
            var c = this.options.c || this.c;
            var r = this.options.r || this.r;
            var id = this.options.id || this.id;
            var that = this; //**correct this..memory leak
            $.get("/posts?r=" + r + "&c=" + c, function(template) {
                that.$el.html(template);
                that.$el.find('#nearlook_real').hide();
                return that;
            });
            return this;
        },
        showForm: function(event) {
            this.$el.find('#nearlook_fake').hide();
            this.$el.find('#nearlook_real').show();
            event.preventDefault();
            console.log(event);
        },
        closePost: function(event) {
            this.$el.find('#nearlook_real').hide();
            this.$el.find('#nearlook_fake').show();
            event.preventDefault();
            console.log(event);
        },
        savePosts: function() {
            debugger;
            var formData = new FormData($("#nearlook_real")[0]);
            var c = $("#cd").val();
            var r = $("#rd").val();
            var url1 = '/addPost/' + c + '/' + r;
            event.preventDefault();
            $.ajax({
                url: url1, //Server script to process data
                type: 'POST',
                success: function(data) {
                    $("#main").html(data);
                }, // Form data
                data: formData, //Options to tell jQuery not to process data or worry about content-type.
                cache: false,
                contentType: false,
                processData: false
            });
        }

    });

    new NlApp.Router;
    Backbone.history.start();
})();