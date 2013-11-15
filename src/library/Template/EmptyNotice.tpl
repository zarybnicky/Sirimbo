{% extends parent %}

{% block content %}
    {% parent() %}
    <h1>
        {{ header }}
    </h1>
    {{ notice }}
{% endblock %}