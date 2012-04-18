/* CSS
<style type="text/css">
div#TopFeaturesBlock {background:url(/images/2.1/background_topfeatures2.gif) #013366 top left repeat-x; width:720px; height:29em; min-height:316px; position:relative;}
div#TopFeaturesBlock div.Feature {width:700px; height:29em; min-height:316px; position:absolute; top:0; left:20px; background:url(/images/2.1/topfeatureimageshadow4.png) scroll no-repeat 256px 8px;}
div#TopFeaturesBlock div.Feature p.FeatureBlurb {color:#fff; width:240px; float:left; padding:0 0 1em; font-size:1.1em;} /* Use Padding instead of Margin to fix IE6 Double Margin Bug
div#TopFeaturesBlock ul.FeatureLinks {float:left; width:240px; padding:0 0; list-style:none;} /* Use Padding instead of Margin to fix IE6 Double Margin Bug
div#TopFeaturesBlock ul.FeatureLinks li {float:left; width:78px; background:url(/images/2.1/background_topfeaturesbutton2.gif) #1c3f65 center left repeat-x; margin-right:3px; border: solid 1px #b2c5d4;}
div#TopFeaturesBlock ul.FeatureLinks li a {display:block; color:#fff; width:78px; text-decoration:none; font-weight:bold; font-size:.9em; text-align:center; padding:.4em 0;}
div#TopFeaturesBlock ul.FeatureLinks li a:hover {color:#5a5a5a; background-color:#fff;}
div#TopFeaturesBlock h2.FeatureTitle {color:#fff; float:left; font-size:1.6em; margin:15px 0 12px; padding:0 0; width:240px;}
div#TopFeaturesBlock div.FeaturePhoto {float:right; margin:12px 12px 0 0 ; width:427px; height:285px;}
div#TopFeaturesBlock div.FeaturePhoto p {text-align:right; color:#fff; margin-top:.2em; font-size:.9em; color:#BFCEE1;}
div#TopFeaturesBlock ul.FeatureIndex {list-style:none; display:block; position:absolute; bottom:22px;}
div#TopFeaturesBlock ul.FeatureIndex > li{float:left; border:solid 1px #fff; background-color:#366998; margin:0 3px;}
div#TopFeaturesBlock ul.FeatureIndex > li:hover{background-color:#7d9cb9;}
div#TopFeaturesBlock ul.FeatureIndex > li.Selected{background-color:#7d9cb9;}
div#TopFeaturesBlock ul.FeatureIndex > li > a{height:14px; width:14px; display:block;}
</style>
<div id="TopFeaturesBlock">
	<div class="Feature" style="display:none;">
		<h2 class="FeatureTitle">
			Titulek
		</h2>
		<div class="FeaturePhoto">
			<img width="427px" height="285px" />
			<p>Popisek fotky</p>
		</div>
		<p class="FeatureBlurb">
			Text článku/preview/summary
		</p>
		<ul class="FeatureLinks">
			<li>
				<a href="">Text linku</a>
			</li>
		</ul>
		<div style="clear:both;"></div>
	</div>
</div>
*/
if (typeof jQuery == "function") {
	(function($) {
		$.fn.topFeatures = function() {
			return this
					.each(function() {
						var k = $(this).children().filter("div");
						//var l = k.children().filter("h2");
						var n = $(this).append('<ul class="FeatureIndex"></ul>').children().filter("ul:last");
						var o = p = q = s = 0;
						var r = {};
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