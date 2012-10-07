<?php
User::checkPermissionsError(L_ADMIN);

if(!empty($_POST) && post('code')) {
	$r = eval(stripslashes(post('code')));
	if($r === FALSE)
		notice('Kód obsahuje syntaktickou chybu');
	else
		notice(dumpVar($r));
}
?>

<form action="<?php echo $_SERVER['REQUEST_URI']; ?>" method="post">
Kód:<br/>
<textarea name="code" rows="10" cols="20"></textarea><br/>
<button type="submit">Zpracovat</button>
</form>