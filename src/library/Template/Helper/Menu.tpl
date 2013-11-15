<div class="sticky" style="width:150px;float:{{ float }}">
	<script type="text/javascript">
	(function($) {
	    $(function() {
	        if (typeof $.fn.topFeatures == "undefined") {
	            $.getScript("/scripts/jquery.sticky.js", function() {
	                $(".sticky").sticky({topSpacing:15});
	            });
	        } else {
	            $(".sticky").sticky({topSpacing:15});
	        }
	    })
	})(jQuery);
	</script>

	<div style="z-index:100;border:1px solid #FFD390;background:#FFE7C7;margin-top:2px;padding:3px 1px;">
	{% for item in data %}
	    {% if item.button %}
		    <button style="padding:0" name="action" value="{{ item.url }}">{{ item.name }}</button>
	    {% else %}
	        <a style="padding:0 3px" href="{{ item.url }}">{{ item.name }}</a>
	    {% endif %}

	    {% if !loop.last %}
		    <br/>
		{% endif %}
	{% endfor %}
	</div>
</div>