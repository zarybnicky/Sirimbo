if (typeof jQuery == "function") {
	(function($) {
		$.fn.dodTopFeatures = function() {
			return this
					.each(function() {
						var k = $(this).children().filter("div");
						//var l = k.children().filter("h2");
						var m = k.find("ul.FeatureLinks a");
						var n = $(this).append('<ul class="FeatureIndex"></ul>').children().filter("ul:last");
						var o = p = q = s = 0;
						var r = {};
						m.click(function() {
							if ($(this).attr("target") == "_blank"
									&& parseInt($(this).attr("dodLinkTargetHeight"), 10) > 0
									&& parseInt($(this).attr("dodLinkTargetWidth"), 10) > 0) {
								window.open(
									$(this).attr("href"),
									"tfWin",
									"width=" + $(this).attr("dodLinkTargetWidth") +
									",height=" + $(this).attr("dodLinkTargetHeight") +
									",resizable=no,scrollbars=no,toolbar=nomenubar=no"
								);
								return false;
							}
						});
						k.each(function(i) {
							var b = $(this).children("div.FeaturePhoto").children("img");
							b.attr("title", b.attr("alt"));
							n.append('<li><a href=\"#\"></a></li>');
							n.children().filter("li:last").children("a:eq(0)")
									.click(function() {
										clearTimer();
										showFeature(this);
										this.blur();
										return false;
									});
							var c = $(this).children("ul.FeatureLinks:first");
							var d = c.children("li");
							var e = (78 + 2) * d.length;
							var f = 0;
							d.last().css("margin-right", "0");
							f = (d.length - 1) * 3;
							c.css("width", e + f);
							var g = parseInt(c.css("padding-right"));
							var h = 240 - (e + f);
							if (h > 2) {
								h = h / 2;
							} else {
								h = 0;
							}
							c.css("padding-right", parseInt(g + h));
							c.css("padding-left", parseInt(g + h));
						});
						r = n.children();
						p = k.eq(0).children().filter("h2:eq(0)").innerWidth();
						q = k.eq(0).position();
						o = n.outerWidth();
						n.css("left", (p / 2) - (o / 2) + q.left);
						n.children().filter("li:first").addClass("Selected");
						function showFeature(a) {
							var b = $(a).parent();
							var c = r.index(b);
							var d = r.filter(".Selected:eq(0)");
							var e = r.index(d);
							if (b.get(0) == d.get(0)) {
								return false;
							}
							d.removeClass("Selected");
							b.addClass("Selected");
							if ($.browser.msie && $.browser.version < 9) {
								$(this).children("div.Feature").css(
										"background-position", "-1000px");
							}
							k.eq(e).fadeOut(1000);
							k.eq(c).fadeIn(1000,
								function() {
									if ($.browser.msie && $.browser.version < 9) {
										$(this).children("div.Feature").css("background-position", "256px 8px");
									}
								});
						}
						function showNextFeature() {
							var a = r.filter(".Selected:eq(0)");
							var b = r.index(a) == (r.length - 1) ? b = r.eq(0) : b = a.next();
							showFeature(b.children("a:eq(0)").get(0));
						}
						function clearTimer() {
							clearTimeout(s);
						}
						if (!k.eq(0).attr("dodTopFeatureStop")) {
							s = setInterval(function() {
								showNextFeature();
							}, 5000);
						}
					});
		};
	})(jQuery);
}