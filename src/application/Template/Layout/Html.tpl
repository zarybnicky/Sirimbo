<!DOCTYPE HTML>
<html prefix="og: http://ogp.me/ns#" lang="cs">
<head>
    <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
    <meta name="keywords"
        content="taneční klub, tk, olymp, olomouc, tk olymp, sportovní tanec" />

    <meta name="ICBM" content="49.591700,17.285174"/>
    <meta name="geo.placename" content="Olomouc, Česká Republika"/>
    <meta name="geo.position" content="49.591700;17.285174"/>
    <meta name="geo.region" content="cs"/>

    <link rel="shortcut icon" type="image/x-icon" href="/favicon.ico" />

    {% block style %}
    <link href="/style/style-tisk.css" rel="stylesheet" type="text/css" />
    {% endblock style %}

    <title>{% block title %}TK Olymp Olomouc{% endblock %}</title>

    <meta property="fb:app_id" content="132983570203245"/>

    <meta name="wot-verification" content="ec0cf41ab42dae52d3d4"/>
    <meta name="msvalidate.01" content="7BD6C8B5748FC22EF06AB3AE89900885" />
    <meta name="google-site-verification" content="Hfe7zlgTDOIpJv4rKGQz2Xg8Aezb6sIO0aAxVhrml9w" />
    <meta name="norton-safeweb-site-verification"
        content="r44xj2vskhlgkyqcqm1hdgga2jdfj-idvyys0277y96s72k-tq0z-yyjdu7h3el6pi2gek0i4ykq3xgiguufrvuhj8nbj4n4miwjhvumhp35jfrafyynhj4ee8ctzpzh" />

    <script type="text/javascript" src="/scripts/jquery-1.10.2.min.js"></script>
    <script type="text/javascript"
        src="//connect.facebook.net/cs_CZ/all.js#xfbml=1&appId=132983570203245"
        id="facebook-jssdk"></script>
    <script type="text/javascript">
    var _gaq = _gaq || [];
    _gaq.push(['_require','inpage_linkid','//www.google-analytics.com/plugins/ga/inpage_linkid.js'],
    ['_setAccount','UA-44456908-1'],['_trackPageview']);
    (function() {var ga = document.createElement('script');ga.type='text/javascript';ga.async=true;
    ga.src='//www.google-analytics.com/ga.js';var s=document.getElementsByTagName('script')[0];s.parentNode.insertBefore(ga,s);
    })();
    </script>
</head>
<body>
<div id="fb-root"></div>
<div id="all">
    <div id="header">
        <a href="/home" id="homelink"></a>
        <div id="userbox">
            {% block userbox %}
            <form action="" method="post">
                <div>
	                <button type="submit" name="action" value="enter" style="display:none;"></button>
	                <input style="width:70px;" type="text" name="login"
	                /><input style="width:70px;" type="password" name="pass" /><br />
	                <a href="/registrace" id="register">Registrace</a>&nbsp;
	                <a href="/nopassword">Nové heslo</a>
	                <button type="submit" name="action" value="login">Login</button>
                </div>
            </form>
            {% endblock %}
        </div>
        <div id="logo">
            Taneční klub Olymp Olomouc
        </div>
        <div id="topmenu">
            {% block topmenu %}
                <ul>
                    <li class="first{{ page == 'home' ? ' current' }}">
                        <a href="/home">Domů</a>
                    </li>
                    <li {{ page == 'oklubu' ? ' class="current"' }}>
                        <a href="/oklubu">O klubu</a>
                    </li>
                    <li {{ page == 'aktualne' ? ' class="current"' }}>
                        <a href="/aktualne">Aktuálně</a>
                    </li>
                    <li {{ page == 'fotogalerie' ? ' class="current"' }}>
                        <a href="/fotogalerie">Fotogalerie</a>
                    </li>
                    <li {{ page == 'nabizime' ? ' class="current"' }}>
                        <a href="/nabizime">Nabízíme</a>
                    </li>
                    <li {{ page == 'member' ? ' class="current"' }}>
                        <a href="/member/home">Členové</a>
                    </li>
                    <li class="last{{ page == 'kontakt' ? ' current' }}">
                        <a href="/kontakt">Kontakt</a>
                    </li>
                </ul>
            {% endblock %}
        </div>
    </div>
    <div id="main">
        <div class="container">
            <div id="sidebar">
                {% block sidemenu %}{% endblock %}
            </div>
            <div id="content">
                {% block toolbox %}
                <div style="float:right;clear:both;">
                    <a href="{{  }}">Tisknout</a>
                </div>
                <div style="clear:both;"></div>
                {% endblock %}
                {% block content %}{% endblock %}
            </div>
        </div>
    </div>
</div>
<div id="footer">
    <div class="container">
        <a href="/admin/home">Administrace</a> | Copyright &copy; 2013,
        TK Olymp Olomouc | realizace: <a href="mailto:kuba.zarybnicky@post.cz">Jakub Zárybnický</a>,
        design: <a href="http://www.linnetdesign.cz">Linnet Design</a>
    </div>
</div>
</body>
</html>
