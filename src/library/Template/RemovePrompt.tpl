{% extends parent %}

{% block content %}
    {% parent() %}
	<h1>
	    {{ header }}
	</h1>
	<form action="" method="POST">
		{{ prompt }}
		<br/>
		<br/>
		{% for item in data %}
		    {{ item.text }}
		    <input type="hidden" name="data[]" value="{{ item.id }}" />
		    <br />
		{% endfor %}
		<br/>
		<button type="submit" name="action" value="confirm">
		    Odstranit
		</button>
		<a href="{{ returnURL }}">
		   Zpět
	    </a>
	</form>
{% endblock %}