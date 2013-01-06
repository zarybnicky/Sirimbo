<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">

<html xmlns="http://www.w3.org/1999/xhtml">
<head>
	<meta name="keywords" lang="cs" content="taneční klub, tk, olymp, olomouc" />
	<meta http-equiv="content-type" content="text/html; charset=utf-8" />
	<title>TK Olymp Olomouc</title>
	<link href="style.css" rel="stylesheet" type="text/css" />
</head>
<body>
<div id="all">
	<div id="header">
		<div id="userbox">
		<?php		
		if(true || !User::isLogged()) {
			if(isset($_POST["login"])) {
				$_POST["pass"] = User::crypt($_POST["pass"]);
			
				if(User::login($_POST["login"], $_POST["pass"])) {
					header("Location: /member/home");
					return;
				}
			}
		?>
		
			<form action="<?php echo $_SERVER["REQUEST_URI"] ?>" method="post">
				<div>
				<input style="width:55px;" type="text" name="login"
				/><input style="width:55px;" type="password" name="pass"
				/><button type="submit" name="action" value="login">Login</button>
				</div>
			</form>
		<?php
		} else {
			echo User::getUserName();
		}?>
		</div>
		<div id="logo">
			Taneční klub Olymp
		</div>
		<div id="topmenu">
			<ul>
				<li class="first"><a href="#">Domů</a></li>
				<li><a href="#">O klubu</a></li>
				<li><a href="#">Aktuálně</a></li>
				<li><a href="#">Inzerce</a></li>
				<li><a href="#">Fotogalerie</a></li>
				<li><a href="#">Nabízíme</a></li>
				<li class="last"><a href="#">Kontakt</a></li>
			</ul>
		</div>
	</div>
	<div id="main">
		<div class="container">
			<div id="sidebar">
				<div class="dark-out"><div class="dark-in"><span class="logo"></span>Menu</div></div>
				<div id="sidemenu">
					<ul>
						<li><a href="#"><span class="arrow">.</span>Noví zájemci</a></li>
						<li><a href="#"><span class="arrow">.</span>Rada klubu</a></li>
						<li><a href="#"><span class="arrow">.</span>Historie</a></li>
						<li><a href="#"><span class="arrow">.</span>Mistroství ČR</a></li>
						<li><a href="#"><span class="arrow">.</span>Taneční liga</a></li>
						<li><a href="#"><span class="arrow">.</span>Odkazy</a></li>
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
				Content
			</div>
		</div>
	</div>
</div>
<div id="footer">
	<div class="container">
		Copyright &copy; 2011, TK Olymp Olomouc | realizace: Jakub Zárybnický, design: Linnet Design
	</div>
</div>
</body>
</html>