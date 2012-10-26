<?php
header_main("Registrace");

global $isConfirmation;
$isConfirmation = post("username") && post("pass") && post("email") && post("telefon");

ob_start();
?>
<form action="<?php echo $_SERVER["REQUEST_URI"] ?>" method="POST">
<table>
	<tr>
		<td>Uživatel (*): </td>
		<td><input type="text" name="username" value="<?php echo post("username"); ?>" /></td>
		<td>Pouze písmena bez diakritiky, číslice a podtržítka, 3 - 20 znaků<?php
			$error = checkPostField("/^[A-Z0-9_]{3,20}$/i", "username");
		?></td>
	</tr><tr>
		<td>Heslo (*): </td>
		<td><input type="password" name="pass" value="<?php echo post("pass"); ?>"/></td>
		<td>Pouze písmena bez diakritiky, číslice a podtržítka, 6 - 32 znaků<?php
			$error = checkPostField("/^[A-Z0-9_]{6,32}$/i", "pass") || $error;
		?></td>
	</tr><tr>
		<td>Jméno: </td>
		<td><input type="text" name="jmeno" value="<?php echo post("jmeno"); ?>" /></td>
		<td>Max. 40 znaků<?php
			$error = checkPostFieldLength(0, 40, "jmeno") || $error;
		?></td>
	</tr><tr>
		<td>Příjmení: </td>
		<td><input type="text" name="prijmeni" value="<?php echo post("prijmeni"); ?>" /></td>
		<td>Max. 40 znaků<?php
			$error = checkPostFieldLength(0, 40, "prijmeni") || $error;
		?></td>
	</tr><tr>
		<td>Pohlaví: </td>
		<td><input type="radio" name="pohlavi" value="m" <?php
			if(post("pohlavi") == "m") echo "checked=\"checked\"";
		?>/>Muž&nbsp;&nbsp;<input type="radio" name="pohlavi" value="f" <?php
			if(post("pohlavi") == "f") echo "checked=\"checked\"";
		?>/>Žena</td>
	</tr><tr>
		<td>E-mail (*): </td>
		<td><input type="text" name="email" value="<?php echo post("email"); ?>" /></td>
		<td>Max. 50 znaků, ve formátu e-mail adresy<?php
			$error = checkPostField("/^\b[A-Z0-9._%-]+@[A-Z0-9.-]+\.[A-Z]{2,4}\b$/i", "email") || $error;
		?></td>
	</tr><tr>
		<td>Telefon (*): </td>
		<td><input type="text" name="telefon" value="<?php echo post("telefon"); ?>" /></td>
		<td><?php
			$error = checkPostField("/^((\+|00)\d{3})?( ?\d{3}){3}$/", "telefon") || $error;
		?></td>
	</tr><tr>
		<td>Datum: (*)</td>
		<td><?php echoDateSelect("day", "month", "year", 1920);?></td>
		<td><?php
			$od_str = post("year") . "-" . post("month") . "-" .
				post("day");
			$error = checkDateString($od_str) || $error;
		?></td>
	</tr><tr>
		<td>Poznámky: </td>
		<td><textarea rows="15" cols="35" name="poznamky"><?php echo post("poznamky"); ?></textarea></td>
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
	if(!DBUser::getUserID(post("username"))) {
		$narozeni = post('year') . '-' . post('month') . '-' . post('day');
		
		User::register(post("username"), post("pass"), post("jmeno"), post("prijmeni"),
			post('pohlavi'), post("email"), post("telefon"), $narozeni, post("poznamky"));
		header("Location: /done");
		echo $html;
	} else {
		notice("Už tu nekdo s takovým přihlašovacím jménem je :o(");
		echo $html;
	}
} else {
	echo $html;
}
?>