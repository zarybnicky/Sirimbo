<select name="{{ name }}">
{% for value, name in options %}
    <option value="{{ value }}"
        {%- if selected == value -%}
            selected="selected"
        {%- endif -%}>
        {{- name -}}
    </option>
{% endfor %}
</select>