<?php
$error = false;

global $isConfirmation;
$isConfirmation = (post("jmeno") || post("prijmeni"));

header_minor("Osobní údaje");
?>
<form action="<?php echo $_SERVER["REQUEST_URI"] ?>" method="POST">
<table>
	<tr>
		<td>Uživatel (*): </td>
		<td><?php echo post("login"); ?></td>
		<td></td>
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
		<td>E-mail: </td>
		<td><input type="text" name="email" value="<?php echo post("email"); ?>" /></td>
		<td>Max. 50 znaků, ve formátu e-mail adresy<?php
			$error = (strlen(post("email")) < 1 ? 0 : 
				checkPostField("/^\b[A-Z0-9._%-]+@[A-Z0-9.-]+\.[A-Z]{2,4}\b$/i", "email")) || $error;
		?></td>
	</tr><tr>
		<td>Telefon: </td>
		<td><input type="text" name="telefon" value="<?php echo post("telefon"); ?>" /></td>
		<td>Pouze číslice a +, max. 13 znaků<?php
			$error = (strlen(post("telefon")) < 1 ? 0 : 
				checkPostField("/^[0-9+]{9,13}$/", "telefon")) || $error;
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
		<td><textarea rows="15" cols="50" name="poznamky"><?php echo post("poznamky"); ?></textarea></td>
		<td></td>
	</tr><tr>
		<td>Uzamknout: </td>
		<td><input type="checkbox" name="lock" value="lock" readonly <?php
			if(post("lock")) echo "checked=\"checked\"";
		?>/></td>
		<td></td>
	</tr><tr>
		<td>Oprávnění: </td>
		<td><?php if(post("level") == "user")
					echo "Uživatel";
				else if(post("level") == "editor")
					echo "Editor";
				else if(post("level") == "trener") 
					echo "Trenér";
				else if(post("level") == "admin")
					echo "Administrátor";
				else if(post("level") == "sadmin")
					echo "Superuživatel";
			?></td>
		<td></td>
	</tr><tr>
		<td><button type="submit" name="action" value="confirm">Upravit</button></td>
		<td></td>
		<td>Pole označená (*) jsou povinná</td>
	</tr>
</table>
</form>

<a href="/member/profil">Zpět</a>