<?php
header_main("Registrace");

global $isConfirmation;
$isConfirmation = getPostField("username") && getPostField("pass");

ob_start();
?>
<form action="<?php echo $_SERVER["REQUEST_URI"] ?>" method="POST">
<table>
	<tr>
		<td>Uživatel (*): </td>
		<td><input type="text" name="username" value="<?php echo getPostField("username"); ?>" /></td>
		<td>Pouze písmena bez diakritiky, číslice a podtržítka, 3 - 20 znaků<?php
			$error = checkPostField("/^[A-Z0-9_]{3,20}$/i", "username");
		?></td>
	</tr><tr>
		<td>Heslo (*): </td>
		<td><input type="password" name="pass" value="<?php echo getPostField("pass"); ?>"/></td>
		<td>Pouze písmena bez diakritiky, číslice a podtržítka, 6 - 32 znaků<?php
			$error = checkPostField("/^[A-Z0-9_]{6,32}$/i", "pass") || $error;
		?></td>
	</tr><tr>
		<td>Jméno: </td>
		<td><input type="text" name="jmeno" value="<?php echo getPostField("jmeno"); ?>" /></td>
		<td>Max. 40 znaků<?php
			$error = checkPostFieldLength(0, 40, "jmeno") || $error;
		?></td>
	</tr><tr>
		<td>Příjmení: </td>
		<td><input type="text" name="prijmeni" value="<?php echo getPostField("prijmeni"); ?>" /></td>
		<td>Max. 40 znaků<?php
			$error = checkPostFieldLength(0, 40, "prijmeni") || $error;
		?></td>
	</tr><tr>
		<td>Pohlaví: </td>
		<td><input type="radio" name="pohlavi" value="m" <?php
			if(getPostField("pohlavi") == "m") echo "checked=\"checked\"";
		?>/>Muž&nbsp;&nbsp;<input type="radio" name="pohlavi" value="f" <?php
			if(getPostField("pohlavi") == "f") echo "checked=\"checked\"";
		?>/>Žena</td>
	</tr><tr>
		<td>E-mail: </td>
		<td><input type="text" name="email" value="<?php echo getPostField("email"); ?>" /></td>
		<td>Max. 50 znaků, ve formátu e-mail adresy<?php
			$error = (strlen(getPostField("email")) < 1 ? 0 : 
				checkPostField("/^\b[A-Z0-9._%-]+@[A-Z0-9.-]+\.[A-Z]{2,4}\b$/i", "email")) || $error;
		?></td>
	</tr><tr>
		<td>Telefon: </td>
		<td><input type="text" name="telefon" value="<?php echo getPostField("telefon"); ?>" /></td>
		<td>Pouze číslice a +, max. 13 znaků<?php
			$error = (strlen(getPostField("telefon")) < 1 ? 0 : 
				checkPostField("/^[0-9+]{9,13}$/", "telefon")) || $error;
		?></td>
	</tr><tr>
		<td>Poznámky: </td>
		<td><textarea rows="15" cols="35" name="poznamky"><?php echo getPostField("poznamky"); ?></textarea></td>
		<td></td>
	</tr><tr>
		<td><button type="submit" name="action" value="register">Registrovat</button></td>
		<td></td>
		<td>Pole označená (*) jsou povinná</td>
	</tr>
</table>
</form>
<?php
$html = ob_get_clean();

if($isConfirmation && !$error) {
	if(!DBUser::getUserID(getPostField("username"))) {
		header("Location: /done");
		User::register(getPostField("username"), User::Crypt(getPostField("pass")),
			getPostField("jmeno"), getPostField("prijmeni"), getPostField('pohlavi'),
			getPostField("email"), getPostField("telefon"), getPostField("poznamky"));
	} else {
		notice("Už tu nekdo s takovým přihlašovacím jménem je :o(");
		echo $html;
	}
} else {
	echo $html;
}
?>