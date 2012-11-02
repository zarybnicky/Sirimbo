
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">

<html xmlns="http://www.w3.org/1999/xhtml">
<head>
	<meta name="keywords" lang="cs" content="taneční klub, tk, olymp, olomouc" />
	<meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
	<link rel="shortcut icon" type="image/x-icon" href="/favicon.ico" />
	<title>TK Olymp Olomouc</title>
	<link href="/style/style.css" rel="stylesheet" type="text/css" />
	<script src="/scripts/jquery-1.7.1.min.js"></script>
</head>
<body>
<div id="all">
	<div id="header">
		<div id="userbox">
		<?php
		if(!User::isLogged()) {
			//TODO: AJAX
			if(post('action') == 'login' ||
					post('action') == 'enter') {
				post('pass', User::crypt(post('pass')));
			
				if(User::login(post('login'), post('pass'))) {
					View::redirect('/member/home');
				} else {
					View::redirect('/login', 'Špatné jméno nebo heslo!', true);
				}
			}
		?>
			<form action="<?php echo $_SERVER['REQUEST_URI'] ?>" method="post"><div>
				<button type="submit" name="action" value="enter" style="display:none;"></button>
				<input style="width:70px;" type="text" name="login"
				/><input style="width:70px;" type="password" name="pass" /><br />
				<a href="/registrace" id="register">Registrace</a>&nbsp;
				<a href="/nopassword">Nové heslo</a>
				<button type="submit" name="action" value="login">Login</button>
			</div></form>
		<?php
		} else {
			echo User::getUserWholeName(), "<br />";
			echo '<a href="/member/profil/platby">Platby</a> ',
				'<a href="/member/profil">Profil</a> ',
				'<a href="/logout">Odhlásit se</a>';
		}
		$section = Request::getSection('home');
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
					$section = Request::getSection('home');
					$s_part = Request::getAction();
					
					$menu = array();
					if($section == 'fotogalerie') {
						include('files/Main/Fotogalerie/Menu.inc');
						echo getSideMenu();
					} else {
						if(array_key_exists($section, Settings::$sekce)) {
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
							if($s_part != $url  && !strpos($url, '*')) {
								echo '<li><a href="/', $section, '/', $url,
									'"><span class="arrow">.</span>', $word,
									'</a></li>', "\n";
							} elseif(strpos($url, '*') !== false) {
								$url_part = substr($url, 0, strlen($url) - 2);
								if(strpos($s_part, $url_part) !== false || $s_part == $url_part)
									echo '<li class="current"><a href="/', $section, '/', $url_part,
										'"><span class="arrow">.</span>', $word,
										'<span class="point">.</span></a></li>', "\n";
								else
									echo '<li><a href="/', $section, '/', $url_part,
										'"><span class="arrow">.</span>', $word,
										'</a></li>', "\n";
							} else {
								echo '<li class="current"><a href="/', $section, '/', $url,
									'"><span class="arrow">.</span>', $word,
									'<span class="point">.</span></a></li>', "\n";
							}
						}
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
			<form action="<?php echo $_SERVER['REQUEST_URI'];?>" method="get">
				<?php
					foreach($_GET as $key => $value) {
						if($key != 'file' && $key != 'tisk')
							echo '<input type="hidden" name="', $key, '" value="', $value, '"/>';
					}
				?>
				<div style="float:right">
				<button type="submit" name="view" value="tisk">Tisknout</button>
				</div>
			</form>