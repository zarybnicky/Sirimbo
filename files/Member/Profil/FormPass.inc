<?php
$error = false;

global $isConfirmation;
$isConfirmation = (post("oldpass") || post("newpass"));

header_minor("Změna hesla");
?>
<form action="<?php echo $_SERVER["REQUEST_URI"];?>" method="post">
<table>
<tr><td>Staré heslo:</td><td><input type="password" name="oldpass" /></td></tr>
<tr><td>Nové heslo:</td><td><input type="password" name="newpass" /></td><td><?php
	$error = checkPostField("/^[A-Z0-9_]{6,32}$/i", "newpass") || $error;
	?></td>
</tr>
<tr><td>Znovu nové heslo:</td><td><input type="password" name="newpass_confirm" /></td><td><?php
	$error = checkPostField("/^[A-Z0-9_]{6,32}$/i", "newpass_confirm") || $error; ?></td>
</tr>
</table>
Pouze písmena bez diakritiky, číslice a podtržítka, 6 - 32 znaků<br />
<button type="submit" name="action" value="edit-pass_confirm">Změnit heslo</button>
</form>

<a href="/member/profil">Zpět</a>