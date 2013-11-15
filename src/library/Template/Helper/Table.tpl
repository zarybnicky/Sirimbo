<table{{ style }}">
    {% if showHeader %}
    <thead>
        <tr>
            {% for item in columns %}
            <th>{{ item.name }}</th>
            {% endfor %}
        </tr>
    </thead>
    {% endif %}
    <tbody>
        {% for item in data if item is not empty %}
        <tr>
            {% for column in columns %}
            <td{{ column.style }}>
                {{ item.(column.dataKey) }}
            </td>
            {% endfor %}
        </tr>
        {% endfor %}
    </tbody>
</table>