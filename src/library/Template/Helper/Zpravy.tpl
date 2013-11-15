<div class="zpravy" style="width:250px;padding:5px;height:396px;border:1px solid #FFB030;border-radius:15px;">
    <h2>
        Krátké zprávy
    </h2>
    <div style="overflow-y:scroll;padding:4px;height:345px;">
    {% for item in data if loop.index0 < count %}
        <div>
            <span class="big">
                {{ item.name }}
            </span>
            <span class="little">
                &nbsp;({{ item.date }})
            </span>
            <p style="text-align:left;">
                {{ item.text }}
            </p>
        </div>
        {% if !loop.last %}
        <hr/>
        {% endif %}
    {% else %}
        <div class="notice">Žádné zprávy</div>
    {% endfor %}
    </div>
</div>