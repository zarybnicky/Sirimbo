<?php
$date = (int) date("md");
if ($date < 320)
    $season = 'zima';
else if ($date < 621)
    $season = 'jaro';
else if ($date < 922)
    $season = 'leto';
else
    $season = 'podzim';

$s = Request::getSection('home');

$tiskURI = Request::getURI() . '?' . http_build_query(array_merge($_GET, array('view' => 'tisk')));

/*
 * $this-> vars: -------------- season s sidebar tiskURI content
 */
?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
    "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
<meta name="keywords" lang="cs"
    content="taneční klub, tk, olymp, olomouc"
/>
<meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
<link rel="shortcut icon" type="image/x-icon" href="/favicon.ico" />
<title>TK Olymp Olomouc</title>
<style type="text/css">
body {
	background-color: white;
	background-image: url('/style/<?php echo $this->season; ?>/logo-bg.png');
	background-repeat: repeat-x;
}

#header {
	background-image: url('/style/<?php echo $this->season; ?>/logo.png');
	background-repeat: no-repeat;
}
</style>
<link href="/style/style.css" rel="stylesheet" type="text/css" />
<meta name="google-site-verification"
    content="Hfe7zlgTDOIpJv4rKGQz2Xg8Aezb6sIO0aAxVhrml9w"
/>
<meta name="norton-safeweb-site-verification"
    content="r44xj2vskhlgkyqcqm1hdgga2jdfj-idvyys0277y96s72k-tq0z-yyjdu7h3el6pi2gek0i4ykq3xgiguufrvuhj8nbj4n4miwjhvumhp35jfrafyynhj4ee8ctzpzh"
/>
<meta property="fb:app_id" content="132983570203245" />
<script src="/scripts/jquery-1.10.2.min.js"></script>
</head>
<body>
    <div id="fb-root"></div>
    <script>(function(d, s, id) {
  var js, fjs = d.getElementsByTagName(s)[0];
  if (d.getElementById(id)) return;
  js = d.createElement(s); js.id = id;
  js.src = "//connect.facebook.net/cs_CZ/all.js#xfbml=1&appId=132983570203245";
  fjs.parentNode.insertBefore(js, fjs);
}(document, 'script', 'facebook-jssdk'));</script>
    <div id="all">
        <div id="header">
            <a href="/home" id="homelink"></a>
            <div id="userbox">
        <?php if (!User::isLogged()): ?>
            <form action="" method="post">
                    <div>
                        <button type="submit" name="action"
                            value="enter" style="display: none;"
                        ></button>
                        <input style="width: 70px;" type="text"
                            name="login"
                        /><input style="width: 70px;" type="password"
                            name="pass"
                        /><br /> <a href="/registrace" id="register">Registrace</a>&nbsp;
                        <a href="/nopassword">Nové heslo</a>
                        <button type="submit" name="action"
                            value="login"
                        >Login</button>
                    </div>
                </form>
        <?php else: ?>
            <?php echo User::getUserWholeName(); ?><br /> <a
                    href="/member/profil/platby"
                >Platby</a> <a href="/member/profil">Profil</a> <a
                    href="/logout"
                >Odhlásit se</a>
        <?php endif; ?>
        </div>
            <div id="logo">Taneční klub Olymp Olomouc</div>
            <div id="topmenu">
                <ul>
                    <li
                        class="first<?php echo ($this->s == 'home') ? ' current' : '';?>"
                    ><a href="/home">Domů</a></li>
                    <li
                        <?php echo ($this->s == 'oklubu') ? ' class="current"' : '';?>
                    ><a href="/oklubu">O klubu</a></li>
                    <li
                        <?php echo ($this->s == 'aktualne') ? ' class="current"' : '';?>
                    ><a href="/aktualne">Aktuálně</a></li>
                    <li
                        <?php echo ($this->s == 'fotogalerie') ? ' class="current"' : '';?>
                    ><a href="/fotogalerie">Fotogalerie</a></li>
                    <li
                        <?php echo ($this->s == 'nabizime') ? ' class="current"' : '';?>
                    ><a href="/nabizime">Nabízíme</a></li>
                    <li
                        <?php echo ($this->s == 'member') ? ' class="current"' : '';?>
                    ><a href="/member/home">Členové</a></li>
                    <li
                        class="last<?php echo ($this->s == 'kontakt') ? ' current' : '';?>"
                    ><a href="/kontakt">Kontakt</a></li>
                </ul>
            </div>
        </div>
        <div id="main">
            <div class="container">
                <div id="sidebar">
            <?php $this->sidebar; ?>
            </div>
                <div id="content">
                    <div style="float: right; clear: both;">
                        <a href="<?php echo $this->tiskURI; ?>">Tisknout</a>
                    </div>
                    <div style="clear: both;"></div>
            
                <?php echo $this->content; ?>
            
            </div>
            </div>
        </div>
    </div>
    <div id="footer">
        <div class="container">
            <a href="/admin/home">Administrace</a> | Copyright &copy;
            2013, TK Olymp Olomouc | realizace: <a
                href="mailto:kuba.zarybnicky@post.cz"
            >Jakub Zárybnický</a>, design: <a
                href="http://www.linnetdesign.cz"
            >Linnet Design</a>
        </div>
    </div>
</body>
</html>
