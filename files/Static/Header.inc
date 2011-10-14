<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">

<html xmlns="http://www.w3.org/1999/xhtml">
<head>
	<meta name="keywords" lang="cs" content="taneční klub, tk, olymp, olomouc" />
	<meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
	<title>TK Olymp Olomouc</title>
	<link href="/style/style.css" rel="stylesheet" type="text/css" />
</head>
<body>
<div id="all">
	<div id="header">
		<div id="userbox">
		<?php
		if(!User::isLogged()) {
			//TODO: AJAX
			if(isset($_POST['login'])) {
				$_POST['pass'] = User::crypt($_POST['pass']);
			
				if(User::login($_POST['login'], $_POST['pass'])) {
					header('Location: /member/home');
					return;
				}
			}
		?>
			<form action="<?php echo $_SERVER['REQUEST_URI'] ?>" method="post"><div
		<?php if(isset($_POST["login"])) echo "class=\"badlogin\"";?>>
				<input style="width:55px;" type="text" name="login"
				/><input style="width:55px;" type="password" name="pass" />
				<button type="submit" name="action" value="login">Login</button>
			</div></form>
			<a href="/registrace">Registrace</a>
		<?php
		} else {
			echo User::getUserWholeName(), "<br />";
			echo '<a href="/member/profil">Profil</a> <a href="/logout">Odhlásit se</a>';
		}?>
		</div>
		<div id="logo">
			Taneční klub Olymp
		</div>
		<div id="topmenu">
			<ul>
				<li class="first"><a href="/home">Domů</a></li>
				<li><a href="/oklubu">O klubu</a></li>
				<li><a href="/aktualne">Aktuálně</a></li>
				<li><a href="/inzerce">Inzerce</a></li>
				<li><a href="/foto">Fotogalerie</a></li>
				<li><a href="/nabizime">Nabízíme</a></li>
				<li><a href="/member/home">Členové</a></li>
				<li class="last"><a href="/kontakt">Kontakt</a></li>
			</ul>
		</div>
	</div>
	<div id="main">
		<div class="container">
			<div id="sidebar">
				<div class="dark-out"><div class="dark-in"><span class="logo"></span>Menu</div></div>
				<div id="sidemenu">
					<ul>
				<?php
					$name = explode('/', $_SERVER['REQUEST_URI']);
					if(array_key_exists($name[1], Settings::$sekce)) {
						foreach(Settings::$sekce[$name[1]] as $url => $word) {
							echo '<li><a href="/' . $name[1] . '/' . $url . '"><span class="arrow">.</span>' .
								$word . '</a></li>';
						}
					} else {
						foreach(Settings::$sekce['home'] as $url => $word) {
							echo '<li><a href="/home/' . $url . '"><span class="arrow">.</span>' .
								$word . '</a></li>';
						}
					}
				?>
					</ul>
				</div>
				<div class="dark-out"><div class="dark-in"><span class="logo"></span>Kontakt</div></div>
				<div class="light-out"><div class="light-in">
					<b>TK OLYMP OLOMOUC</b><br />
					<hr />
					Mgr. Marie Hýžová<br />
					telefon: +420 585 312 504<br />
					mobil: +420 604 756 085<br />
					e-mail: tkolymp@tkolymp.cz<br /><br />
					<b>KDE NÁS NAJDETE</b><br />
					<hr />
					Jiráskova 25<br />
					772 00 Olomouc<br />
				</div></div>
			</div>
			<div id="content">
