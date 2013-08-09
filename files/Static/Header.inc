
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">

<html xmlns="http://www.w3.org/1999/xhtml">
<head>
	<meta name="keywords" lang="cs" content="taneční klub, tk, olymp, olomouc" />
	<meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
	<link rel="shortcut icon" type="image/x-icon" href="/favicon.ico" />
	<title>TK Olymp Olomouc</title>
	<style type="text/css">
<?php
	$date = (int) date("md");
	if($date < 320) $dir = 'zima';
	else if($date < 621) $dir = 'jaro';
	else if($date < 922) $dir = 'leto';
	else $dir = 'podzim';
	echo <<<EOS
body{
	background-color:white;
	background-image:url('/style/$dir/logo-bg.png');
	background-repeat:repeat-x;
}#header{
	background-image:url('/style/$dir/logo.png');
	background-repeat:no-repeat;
}
EOS;
?>		
	</style>
	<link href="/style/style.css" rel="stylesheet" type="text/css" />
    <meta name="google-site-verification" content="Hfe7zlgTDOIpJv4rKGQz2Xg8Aezb6sIO0aAxVhrml9w" />
    <meta name="norton-safeweb-site-verification"
    	content="r44xj2vskhlgkyqcqm1hdgga2jdfj-idvyys0277y96s72k-tq0z-yyjdu7h3el6pi2gek0i4ykq3xgiguufrvuhj8nbj4n4miwjhvumhp35jfrafyynhj4ee8ctzpzh" />
	<script src="/scripts/jquery-1.10.2.min.js"></script>	
</head>
<body>
<div id="all">
	<div id="header">
		<a href="/home" id="homelink"></a>
		<div id="userbox">
		<?php
		if(!User::isLogged()) {
			//TODO: AJAX
			if(post('action') == 'login' ||
					post('action') == 'enter') {
				post('pass', User::crypt(post('pass')));
				
				if(!User::login(post('login'), post('pass'))) {
					View::redirect('/login', 'Špatné jméno nebo heslo!', true);
				} elseif(get('return')) {
					View::redirect(get('return'));
				} else {
					View::redirect('/member/home');
				}
			}
		?>
			<form action="<?php Request::getURI();?>" method="post"><div>
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
				<li<?php echo ($section == 'aktualne') ? ' class="current"' : '';
				?>><a href="/aktualne">Aktuálně</a></li>
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
			<?php
				View::$controller->sidebar();
				
				if(!Database::isDatabaseError() && stripos(Request::getURI(), '/home') === false) {
					ob_start();
					$result = DisplayAnkety::viewAnkety(true, true, getIP());
					$ankety = ob_get_clean();
					
					if($result) {
						echo '<div class="dark-out"><div class="dark-in"><span class="logo"></span>',
							'Ankety</div></div><div class="light-out"><div class="light-in">';
						echo $ankety;
						echo '</div></div>';
					}
	                echo '<div><a href="https://www.facebook.com/groups/44213111683/" target="_blank">';
	                echo '<img alt="" src="/style/fb-logo.png" />';
	                echo '</a></div>';
	            }
			?>
                <!--
				<div class="dark-out"><div class="dark-in"><span class="logo"></span>Sponzoři</div></div>
				<div class="light-out"><div class="light-in">
                    
				</div></div>
                -->
			</div>
			<div id="content">
			<form action="<?php echo $_SERVER['REQUEST_URI'];?>" method="get">
				<?php
					foreach($_GET as $key => $value) {
						if($key != 'file' && $key != 'tisk')
							echo '<input type="hidden" name="', $key, '" value="', $value, '"/>';
					}
				?>
				<div style="float:right;clear:both;">
				<button type="submit" name="view" value="tisk">Tisknout</button>
				</div>
				<div style="clear:both;"></div>
			</form>