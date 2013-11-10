<div class="clanky" style="padding:4px 7px;border:1px solid #FFB030;border-radius:15px;">
<h2>Další články</h2>

{% for item in data|slice(offset, count) %}
    <div>
        <span class="big">
            <a href="{{ item.url }}">
                {{ item.name }}
            </a>
        </span>
        <span class="little">
            &nbsp;{{ item.date }})
        </span>
        <p>
            {{ item.preview }}&nbsp;&nbsp;
            <a href="{{ item.url }}">
                pokračujte zde
            </a>
        </p>
    </div>
    {% if not loop.last %}
        <hr/>
    {% endif %}
{% else %}
    <div class="notice">Žádné další články</div>
{% endfor %}
</div>