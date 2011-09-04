<?php
$error = false;

global $isConfirmation;
$isConfirmation = (getPostField("login") || getPostField("pass"));

if(getPostField("action") == "edit" || getPostField("action") == "edit_confirm")
	$header = "Upravit užitatele";
else if(getPostField("action") == "add" || getPostField("action") == "add_confirm")
	$header = "Přidat uživatele";
else
	$header = "Neznámá akce";
header_minor($header);
?>
<form action="<?php echo $_SERVER["REQUEST_URI"] ?>" method="POST">
<table>
	<tr>
		<td>Uživatel (*): </td>
		<td><input type="text" name="login" value="<?php echo getPostField("login"); ?>" <?php
			if(getPostField("action") == "edit" || getPostField("action") == "edit_confirm")
				echo "readonly=\"readonly\"";
		?>/></td>
		<td>Pouze písmena bez diakritiky, číslice a podtržítka, 3 - 20 znaků<?php
			$error = checkPostField("/^[A-Z0-9_]{3,20}$/i", "login");
		?></td>
	</tr><tr>
		<td>Heslo (*): </td>
		<td><input type="text" name="pass" value="<?php echo getPostField("pass"); ?>" <?php
			if(getPostField("action") == "edit" || getPostField("action") == "edit_confirm")
				echo "disabled=\"disabled\"";
		?>/></td>
		<td>Pouze písmena bez diakritiky, číslice a podtržítka, 6 - 32 znaků<?php
			if(getPostField("action") != "edit" && getPostField("action") != "edit_confirm") {
				$error = checkPostField("/^[A-Z0-9_]{6,32}$/i", "pass") || $error;
			}
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
		<td><textarea rows="15" cols="50" name="poznamky"><?php echo getPostField("poznamky"); ?></textarea></td>
		<td></td>
	</tr><tr>
		<td>Uzamknout: </td>
		<td><input type="checkbox" name="lock" value="lock" <?php
			if(getPostField("lock")) echo "checked=\"checked\"";
		?>/></td>
		<td></td>
	</tr><tr>
		<td>Ban: </td>
		<td><input type="checkbox" name="ban" value="ban" <?php
			if(getPostField("ban")) echo "checked=\"checked\"";
		?>/></td>
		<td></td>
	</tr><tr>
		<td>Oprávnění: </td>
		<td><select name="level">
				<option value="user" <?php if(getPostField("level") == "user")
					echo "selected=\"selected\"";?>>Uživatel</option>
				<option value="editor" <?php if(getPostField("level") == "editor")
					echo "selected=\"selected\"";?>>Editor</option>
				<option value="trener" <?php if(getPostField("level") == "trener") 
					echo "selected=\"selected\"";?>>Trenér</option>
				<option value="admin" <?php if(getPostField("level") == "admin")
					echo "selected=\"selected\"";?>>Administrátor</option>
		</select></td>
		<td></td>
	</tr><tr>
		<?php if(getPostField("action") == "edit" || getPostField("action") == "edit_confirm") {?>
		<td><button type="submit" name="action" value="edit_confirm">Upravit</button></td>
		<?php } else if(getPostField("action") == "add" || getPostField("action") == "add_confirm") {?>
		<td><button type="submit" name="action" value="add_confirm">Přidat</button></td>
		<?php }	?>
		<td></td>
		<td>Pole označená (*) jsou povinná</td>
	</tr>
</table>
</form>

<form action="<?php echo $_SERVER["REQUEST_URI"] ?>" method="GET">
<button type="submit">Zpět</button>
</form>