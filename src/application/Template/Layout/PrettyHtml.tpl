{% extends 'Layout/Html.tpl' %}

{% block style %}
<link href="/style/style.css" rel="stylesheet" type="text/css" />
<style type="text/css">
body{
    background-color:white;
    background-image:url("/style/{{ season }}/logo-bg.png");
    background-repeat:repeat-x;
} #header{
    background-image:url("/style/{{ season }}/logo.png");
    background-repeat:no-repeat;
}
</style>
{% endblock %}