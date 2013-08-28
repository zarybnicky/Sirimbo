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
    <meta name="norton-safeweb-site-verification" content="r44xj2vskhlgkyqcqm1hdgga2jdfj-idvyys0277y96s72k-tq0z-yyjdu7h3el6pi2gek0i4ykq3xgiguufrvuhj8nbj4n4miwjhvumhp35jfrafyynhj4ee8ctzpzh" />
</head>
<body>
<div id="all">
	<div id="header">
		<a href="/home" id="homelink"></a>
		<div id="logo">
			Taneční klub Olymp Olomouc
		</div>
		<div id="topmenu">
			<ul>
				<li class="first"><a href="/home">Domů</a></li>
				<li><a href="/oklubu">O klubu</a></li>
				<li><a href="/aktuality/posledni">Aktuálně</a></li>
				<li><a href="/fotogalerie">Fotogalerie</a></li>
				<li><a href="/nabizime">Nabízíme</a></li>
				<li><a href="/member/home">Členové</a></li>
				<li class="last"><a href="/kontakt">Kontakt</a></li>
			</ul>
		</div>
	</div>
	<div id="main">
		<div class="container">
			<table style="width:100%;text-align:center;">
				<tr>
					<td style="background:none;border:none;"><img src="/images/underconstruction.png" alt="" style="margin-left:100px;width:200px;height:auto;" /></td>
					<td valign="middle" style="font-size:large;padding-left:20px;width:700px;background:none;border:none;">Omlouváme se za dočasné nepříjemnosti,<br/>probíhá práce na systému</td>
				</tr>
			</table>
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