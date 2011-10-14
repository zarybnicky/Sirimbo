<?php
$error = false;

global $isConfirmation;
$isConfirmation = (getPostField("oldpass") || getPostField("newpass"));

header_minor("Změna hesla");
?>
<form action="<?php echo $_SERVER["REQUEST_URI"];?>" method="post">
	Staré heslo: <input type="password" name="oldpass" /><br />
	
	Nové heslo: <input type="password" name="newpass" />
	<?php $error = checkPostField("/^[A-Z0-9_]{6,32}$/i", "newpass") || $error; ?><br />
	
	Znovu nové heslo: <input type="password" name="newpass_confirm" />
	<?php $error = checkPostField("/^[A-Z0-9_]{6,32}$/i", "newpass_confirm") || $error; ?><br />
	
	Pouze písmena bez diakritiky, číslice a podtržítka, 6 - 32 znaků<br />
	<button type="submit" name="action" value="edit-pass_confirm">Změnit heslo</button>
</form>

<form action="<?php echo $_SERVER["REQUEST_URI"] ?>" method="GET">
<button type="submit">Zpět</button>
</form>