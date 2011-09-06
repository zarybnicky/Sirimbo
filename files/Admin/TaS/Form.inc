<?php
$error = false;

global $isConfirmation;
$isConfirmation = (getPostField("jmeno") || getPostField("misto"));

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
		<td>Jméno akce (*): </td>
		<td><input type="text" name="jmeno" value="<?php echo getPostField("jmeno"); ?>" /></td>
		<td>Max. 255 znaků<?php
			$error = checkPostFieldLength(1, 255, "jmeno");
		?><input type="hidden" name="id" value="<?php echo getPostField("id");?>" /></td>
	</tr><tr>
		<td>Místo akce (*): </td>
		<td><input type="text" name="kde" value="<?php echo getPostField("kde"); ?>" /></td>
		<td>Max. 255 znaků<?php
			$error = checkPostFieldLength(1, 255, "kde") || $error;
		?></td>
	</tr><tr>
		<td>Další info: </td>
		<td><textarea rows="15" cols="50" name="info"><?php echo getPostField("info"); ?></textarea></td>
		<td></td>
	</tr><tr>
		<td>Od: (*)</td>
		<td><?php echoDate("od_day", "od_month", "od_year");?></td>
		<td><?php
			$od_str = getPostField("od_year") . "-" . getPostField("od_month") . "-" .
				getPostField("od_day");
			$error = checkDateString($od_str) || $error;
		?></td>
	</tr><tr>
		<td>Do: </td>
		<td><?php echoDate("do_day", "do_month", "do_year");?></td>
		<td>(pokud je prázdný, počítá se jako "Od")<?php
			$do_str = getPostField("do_year") . "-" . getPostField("do_month") . "-" .
				getPostField("do_day");
			$error = (($do_str == "0000-00-00") ? 0 : checkDateString($do_str)) || $error;
		?></td>
	</tr><tr>
		<td>Kapacita (*): </td>
		<td><input type="text" name="kapacita" value="<?php echo getPostField("kapacita"); ?>" /></td>
		<td>Pouze číslice<?php
			$error = checkPostField("/^[0-9]{0,11}/", "kapacita") || $error;
		?></td>
	</tr><tr>
		<td>Dokumenty: </td>
		<td><input type="text" name="dokumenty" value="<?php echo getPostField("dokumenty"); ?>" /></td>
		<td><?php //TODO: Dokumenty array
			$error = checkPostField("/^{(\"[a-z0-9.-_]{0,255}\")*}/i", "dokumenty") || $error;
		?></td>
	</tr><tr>
		<td>Uzamknout: </td>
		<td><input type="checkbox" name="lock" value="lock" <?php
			if(getPostField("lock")) echo "checked=\"checked\"";
		?>/></td>
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