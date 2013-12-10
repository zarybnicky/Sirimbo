<div id="{{ divClass }}">

<select name="{{ name }}">
    <option value="none"
        {%- if selected == null -%}
            selected="selected"
        {%- endif -%}>
        --- žádný ---
    </option>

    {% if showTempUserForm %}
    <option value="temporary">--- dočasný ---</option>
    {% endif %}

    {% for item in data %}
        <option value="{{ item.id }}"
            {%- if selected == value -%}
                selected="selected"
            {%- endif -%}>
            {{ item.name }}
            {%- if item.year -%}
                , {{ item.year }}
            {%- endif %}
        </option>
    {% endfor %}
</select>

{% if showTempForm %}
<div class="new" style="display:none;text-align:right;">
    Jméno: <input type="text" name="jmeno" size="8" /><br/>
    Příjmení: <input type="text" name="prijmeni" size="8" /><br/>
    Datum narození:&nbsp;<br/>
    {{ dateSelect }}<br/>
    <button type="submit">Uložit</button>
</div>
<div class="loading" style="display:none;">
    <img alt="Čekám na odezvu serveru..." src="/images/loading_bar.gif"/>
</div>
<script type="text/javascript">
(function($) {
    $(function() {
        if (typeof $.fn.tempUserSelect == "undefined" && typeof window.loadingUS == "undefined") {
            window.loadingUS = true;
            $.getScript("/scripts/tempUserSelect.js", function() {
                $(".{{ divClass }}").tempUserSelect("<?php echo $this->_type;?>");
                delete window.loadingUS;
            });
        } else {
            $.delayed<?php echo $name ?> = function() {
                if (typeof window.loadingUS == "undefined" && typeof $.fn.tempUserSelect != "undefined") {
                    $(".{{ divClass }}").tempUserSelect("<?php echo $this->_type;?>");
                } else {
                    setTimeout(function() {$.delayed<?php echo $name?>();}, 200);
                }
            };
            $.delayed<?php echo $name?>();
        }
    })
}) (jQuery);
</script>
{% endif %}
</div>
