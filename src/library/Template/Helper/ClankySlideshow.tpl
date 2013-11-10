'offset' => $this->offset,
'count'  => $this->count,
'data'   => $this->data

<div id="TopFeaturesBlock">
{% for item in data|slice(offset, count) %}
    <div class="Feature"{% if loop.index0 != offset %} style="display:none;"{% endif %}>
    <h2 class="FeatureTitle">{{ item.name }}</h2>
    <div class="FeaturePhoto">
        <img src="{{ item.photoUrl }}" alt="{{ item.name }}" />
    <p></p>
    </div>
    <p class="FeatureBlurb">
        {{ item.preview }}
    </p>
    <ul class="FeatureLinks">
    <li><a href="{{ item.url }}">Více</a></li>
    </ul>
    <div style="clear:both;"></div>
    </div>
{% endfor %}
</div>

<script type="text/javascript">
(function($) {
    $(function() {
        if (typeof $.fn.topFeatures == "undefined") {
            $.getScript("/scripts/topFeatures.min.js", function() {
                $("#TopFeaturesBlock").topFeatures()
            });
        } else {
            $("#TopFeaturesBlock").topFeatures();
        }
    })
})(jQuery);
</script>