<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">

<html xmlns="http://www.w3.org/1999/xhtml">
<head>
	<meta name="keywords" lang="cs" content="taneční klub, tk, olymp, olomouc" />
	<meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
	<link rel="shortcut icon" type="image/x-icon" href="/favicon.ico" />
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
			<form action="<?php echo $_SERVER['REQUEST_URI'] ?>" method="post"><div<?php
				if(isset($_POST["login"]))
					echo ' class="badlogin"';?>>
				<input style="width:70px;" type="text" name="login"
				/><input style="width:70px;" type="password" name="pass" /><br />
				<a href="/registrace" id="register">Registrace</a>
				<button type="submit" name="action" value="login">Login</button>
			</div></form>
		<?php
		} else {
			echo User::getUserWholeName(), "<br />";
			echo '<a href="/member/profil">Profil</a> <a href="/logout">Odhlásit se</a>';
		}
		
		$name = explode('/', $_SERVER['REQUEST_URI']);
		$section = (isset($name[1])) ? $name[1] : '';
		?>
		</div>
		<div id="logo">
			Taneční klub Olymp Olomouc
		</div>
		<div id="topmenu">
			<ul>
				<li class="first<?php echo ($section == 'home' || $section == '') ? ' current' : '';
				?>"><a href="/home">Domů</a></li>
				<li<?php echo ($section == 'oklubu') ? ' class="current"' : '';
				?>><a href="/oklubu">O klubu</a></li>
				<li<?php echo ($section == 'aktuality') ? ' class="current"' : '';
				?>><a href="/aktuality/posledni">Aktuálně</a></li>
				<li<?php echo ($section == 'inzerce') ? ' class="current"' : '';
				?>><a href="/inzerce/posledni">Inzerce</a></li>
				<li<?php echo ($section == 'fotogalerie') ? ' class="current"' : '';
				?>><a href="/fotogalerie">Fotogalerie</a></li>
				<li<?php echo ($section == 'nabizime') ? ' class="current"' : '';
				?>><a href="/nabizime">Nabízíme</a></li>
				<li<?php echo ($section == 'member') ? ' class="current"' : '';
				?>><a href="/member/home">Členové</a></li>
				<li class="last<?php echo ($section == 'kontakt') ? ' current' : '';
				?>"><a href="/kontakt">Kontakt</a></li>
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
					$uri = $_SERVER['REQUEST_URI'];
					$name = explode('/', $_SERVER['REQUEST_URI']);
					$section = (isset($name[1])) ? $name[1] : '';
					$s_part = (isset($name[2])) ? implode('/', array_slice($name, 2)) : '';
					
					$menu = array();
					if($name[1] == 'fotogalerie') {
						include('files/Main/Fotogalerie/Menu.inc');
						$menu = getSideMenu();
					} else if(array_key_exists($name[1], Settings::$sekce)) {
						$menu = Settings::$sekce[$section];
					} else {
						$section = 'home';
						$menu = Settings::$sekce['home'];
					}
					foreach($menu as $url => $word) {
						if(is_array($word)) {
							if(User::checkPermissionsBool($word[1]))
								$word = $word[0];
							else
								continue;
						}
						if($s_part != $url)
							echo '<li><a href="/', $section, '/', $url,
								'"><span class="arrow">.</span>', $word,
								'</a></li>';
						else
							echo '<li class="current"><a href="/', $section, '/', $url,
								'"><span class="arrow">.</span>', $word,
								'<span class="point">.</span></a></li>';
					}
				?>
					</ul>
				</div>
				<?php
				if(!Database::isDatabaseError()) {
					ob_start();
					$result = DisplayAnkety::viewAnkety(true, true, getIP());
					$ankety = ob_get_clean();
					
					if($result) {
						echo '<div class="dark-out"><div class="dark-in"><span class="logo"></span>',
							'Ankety</div></div><div class="light-out"><div class="light-in">';
						echo $ankety;
						echo '</div></div>';
					}
				}
				?>
				<div class="dark-out"><div class="dark-in"><span class="logo"></span>Kontakt</div></div>
				<div class="light-out"><div class="light-in">
					<b>TK OLYMP OLOMOUC</b><br />
					<hr />
					Mgr. Marie Hýžová<br/>
					- vedoucí klubu<br/>
					tel.: 604 756 085<br/>
					tkolymp(zavináč)tkolymp.cz<br/>
					<br/>
					Miroslav Hýža<br/>
					- informace<br/>
					tel.: 737 545 525<br/>
					hyzam(zavináč)tkolymp.cz<br/>
				</div></div>
			</div>
			<div id="content">
