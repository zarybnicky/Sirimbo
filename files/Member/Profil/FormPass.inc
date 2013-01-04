<?php
header_minor("Změna hesla");

if(isset($f))
	notice(implode('<br/>', $f->getMessages()));
?>
<form action="<?php echo Request::getURI();?>" method="post">
<table>
<tr><td>Staré heslo:</td><td><input type="password" name="oldpass" /></td></tr>
<tr><td>Nové heslo:</td><td><input type="password" name="newpass" /></td></tr>
<tr><td>Znovu nové heslo:</td><td><input type="password" name="newpass_confirm" /></td></tr>
<tr>
	<td><button type="submit" name="action">Změnit heslo</button></td>
	<td>Pouze písmena bez diakritiky, číslice a podtržítka, 6 - 32 znaků</td>
</tr>
</table>
</form>
<a href="/member/profil">Zpět</a>