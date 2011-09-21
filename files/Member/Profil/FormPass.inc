<form action="<?php echo $_SERVER["REQUEST_URI"];?>" method="post">
Staré heslo: <input type="password" name="oldpass" />
Nové heslo: <input type="password" name="newpass" />
Znovu nové heslo: <input type="password" name="newpass_c" />
Pouze písmena bez diakritiky, číslice a podtržítka, 6 - 32 znaků
<?php $error = checkPostField("/^[A-Z0-9_]{6,32}$/i", "pass"); ?>
</form>