<?php
$error = false;

global $isConfirmation;
$isConfirmation = (post("jmeno") || post("prijmeni"));

header_minor("Osobní údaje");
?>
<form action="<?php echo $_SERVER["REQUEST_URI"] ?>" method="POST">
<table>
	<tr>
		<td>Uživatel: </td>
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
		<td>
			<label><?php echo getRadio('pohlavi', 'm');?>Muž</label>&nbsp;
			<label><?php echo getRadio('pohlavi', 'f');?>Žena</label>
		</td>
		<td></td>
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
		<td>Datum narození: (*)</td>
		<td><?php echo Helper::get()->date()->name('narozeni')->textBox(); ?></td>
		<td><?php $error = checkDateString(Helper::get()->date(false)->getPost()) || $error;
		?></td>
	</tr><tr>
		<td>Poznámky: </td>
		<td><textarea rows="15" cols="50" name="poznamky"><?php echo post("poznamky"); ?></textarea></td>
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