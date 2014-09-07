$(function() {
    function delayMasonry() {
        if (Masonry) {
            var m = new Masonry(document.querySelector('#more-articles'), {});
        } else {
            setTimeout(function() { delayMasonry() }, 50);
        }
    }
    delayMasonry();

    var menuHeight = $("#menu").outerHeight();
    var headerHeight = $("#header-inner").outerHeight() + menuHeight - 7;
    var scrollTop = $(window).scrollTop();
    var prevScroll = $(window).scrollTop();

    function fixTopMenu(fast) {
        $('#menu').css("position", "fixed")
            .css("top", "0")
            .css("opacity", "1");
        $('.highlights').css("padding-top", menuHeight + 5);
        $('#menu-logo').stop().animate(
            {
                'opacity': '1'
            },
            {
                'queue': false,
                'start': function() {
                    $(this).css('display', 'block');
                    $('#menu').css('border-bottom', '1px solid #000');
                },
                'duration': fast ? 0 : 500
            }
        );
        $('#menu-inner').stop().animate(
            {
                'padding-left': '30px',
                'width': '930px'
            },
            {
                'queue': false,
                'duration': fast ? 0 : 500
            }
        );
    }

    function releaseTopMenu() {
        $('#menu').css("position", "static")
        $('.highlights').css("padding-top", "0");
        $('#menu-logo').stop().animate(
            {
                'opacity': '0'
            },
            {
                'queue': false,
                'stop': function() {
                    $(this).css('display', 'none');
                    $('#menu').css('border-bottom', 'none');
                }
            }
        );
        $('#menu-inner').stop().animate(
            {
                'padding-left': '0',
                'width': '960px'
            },
            {
                'queue': false,
            }
        );

    }

    function initTopMenu() {
        $(window).scroll(function() {
            scrollTop = $(window).scrollTop();
            if (
                scrollTop >= headerHeight &&
                    prevScroll < headerHeight
            ) {
                fixTopMenu(false);
            } else if(
                scrollTop < headerHeight &&
                    prevScroll >= headerHeight
            ) {
                releaseTopMenu();
            }
            prevScroll = scrollTop;
        });

        if (scrollTop >= headerHeight) {
            fixTopMenu(true);
        }
    }
    initTopMenu();

    function maxHeight(element) {
        var height = 0;
        $(element).each(function() {
            if($(this).height() > height) {
                height = $(this).height();
            }
        });
        return height;
    }
    function minHeight(element) {
        var height = -1;
        $(element).each(function() {
            if($(this).height() < height || height == -1) {
                height = $(this).height();
            }
        });
        return height;
    }
    $(".highlights-info").height(maxHeight($(".highlights-info")));

    $(".highlights li").filter(
        function(index, element) {
            return $(this).find(".highlights-info .desc")
                .is(":not(:empty)");
        }
    ).hover(
        function() {
            var height = minHeight($(this).siblings()
                                   .find(".highlights-info")) + 59;
            $(this).find(".highlights-info").animate(
                {
                    "height": height
                },
                {
                    "start": function() {
                        $(this).find(".desc")
                            .css("opacity", "0")
                            .css("display", "block")
                            .animate(
                                {
                                    "opacity" : "1"
                                }
                            );
                    },
                    "duration": 250
                }
            );
        },
        function() {
            var height = minHeight($(this).siblings()
                                   .find(".highlights-info"));
            $(this).find(".highlights-info")
                .animate(
                    {
                        "height": height
                    },
                    {
                        "start": function() {
                            $(this).find(".desc").fadeOut(100);
                        },
                        "duration": 250
                    }
                );
        }
    );
});
