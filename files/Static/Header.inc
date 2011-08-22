<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">

<html xmlns="http://www.w3.org/1999/xhtml">
<head>
	<title>TK Olymp Olomouc</title>
	<meta name="keywords" lang="cs" content="taneční klub, tk, olymp, olomouc" />
	<meta http-equiv="content-type" content="text/html; charset=utf-8" />
	<style type="text/css">
		div#menu {
			float: left;
			width: 200px;
		} div#content {
			margin-left: 200px;
		}
	</style>
</head>
<body>
<div id="menu">
<ul class="menu">
	<li><a href="/home">Domů</a></li>
	<li>
		<a href="/onas">O nás</a>
		<ul id="submenu">
			<li><a href="/onas/historie">Historie</a></li>
			<li><a href="/onas/treninky">Tréninky</a></li>
			<li><a href="/onas/jine">Jiné</a></li>
		</ul>
	</li>
	<li><a href="/nabor">Pro nové zájemce</a></li>
	<li><a href="/ukazky">Vystoupení</a></li>
	<li><a href="/foto">Fotogalerie</a></li>
	<li><a href="/pary">Naše páry</a></li>
	<li><a href="/forum">Fórum</a></li>
	<li><a href="/inzerce">Inzerce</a></li>
</ul>
<?php if(User::isLogged()) { ?>
<div>
<div>Členové</div>
<ul>
<li><a href="/member/home">Novinky</a></li>
<li><a href="/member/nastenka">Nástěnka</a></li>
<li><a href="/member/rozpis">Rozpis tréninků</a></li>
<li><a href="/member/nabidka">Tréninková nabídka</a></li>
<li><a href="/member/tabory">Tábory a soustředění</a></li>
<li><a href="/member/dokumenty">Dokumenty</a></li>
<li><a href="/member/zebricek">Žebříček párů</a></li>
</ul>
<ul>
<li><a href="/member/profil">Profil</a></li>
<li><a href="/member/logout">Odhlásit se</a></li>
</ul>
</div>
<?php
	if(User::checkPermissionsBool(L_ADMIN)) {
		//TODO: Editor, trener menu
?>
<div>
<div>Administrace</div>
<ul>
<li><a href="/admin/users">Uživatelé</a></li>
<li><a href="/admin/nastenka">Nástěnka</a></li>
<li><a href="/admin/rozpis">Rozpis</a></li>
<li><a href="/admin/nabidka">Nabídka</a></li>
<li><a href="/admin/dokumenty">Dokumenty</a></li>
</ul>
</div>
<?php
	} elseif(User::checkPermissionsBool(L_EDITOR)) {
?>
<div>
<div>Administrace</div>
<ul>
<li><a href="/admin/nastenka">Nástěnka</a></li>
<li><a href="/admin/dokumenty">Dokumenty</a></li>
</ul>
</div>
<?php
	} elseif(User::checkPermissionsBool(L_TRENER)) {
?>
<div>
<div>Administrace</div>
<ul>
<li><a href="/admin/nastenka">Nástěnka</a></li>
<li><a href="/admin/rozpis">Rozpis</a></li>
<li><a href="/admin/nabidka">Nabídka</a></li>
<li><a href="/admin/dokumenty">Dokumenty</a></li>
</ul>
</div>
<?php
	}
} else {
	//TODO: AJAX
	if(isset($_POST["login"])) {
		if(!isset($_POST["ajax"]))
			$_POST["pass"] = md5($_POST["pass"]);
	
		if(User::login($_POST["login"], $_POST["pass"])) {
			header("Location: /member/home");
			return;
		}
	}
?>
<form action="<?php echo $_SERVER["REQUEST_URI"] ?>" method="post">
	<table style="width:125px;" cellspacing="0" cellpadding="0">
		<tr>
			<td class="roh_lh">&nbsp;</td>
			<td class="roh_ph">&nbsp;</td>
		</tr>
		<tr>
			<td>
				<input style="width:100px;" type="text" name="login" />
				<input style="width:100px;" type="password" name="pass" />
			</td>
			<td align="center">
				<input type="submit" value="Login" />
			</td>
		</tr>
		<tr>
			<td class="roh_ld"></td>
			<td class="roh_pd"></td>
		</tr>
	</table>
</form>
<?php
	if(isset($_POST["login"])) {
?>
<div style="border:1px solid red;">
Špatné jméno nebo heslo
</div>
<?php
	}
} ?>
</div>
<div id="content">
<hr />