<?php
$error = false;

global $isConfirmation;
$isConfirmation = (getPostField("trener") || getPostField("pocet_hod"));

echo "<h4>";
if(getPostField("action") == "edit" || getPostField("action") == "edit_confirm")
	echo "Upravit nabídku";
else if(getPostField("action") == "add" || getPostField("action") == "add_confirm")
	echo "Přidat nabídku";
else
	echo "Neznámá akce";
echo "</h4>";
?>
<form action="<?php echo $_SERVER["REQUEST_URI"] ?>" method="POST">
<table>
	<tr>
		<td>Trenér (*): </td>
		<td><?php
		if(!User::checkPermissionsBool(L_ADMIN)) { ?>
		<select name="trener" disabled="disabled">
			<option value="<?php User::getUserID()?>">Já</option>
		</select><?php
		} else { ?>
		<select name="trener">
			<option value="none" <?php if(!getPostField("trener")) echo "selected=\"selected\"";?>>
				Vyber si :o)</option>
			<?php
			$treneri = Database::getTrener();
			foreach($treneri as $item) {
				echo "\t\t\t<option value=\"" . $item["u_id"] . "\"";
				if(getPostField("trener") == $item["u_id"])
					echo " selected=\"selected\"";
				echo ">" . $item["u_jmeno"] . " " . $item["u_prijmeni"];
				echo "</option>\n";
			} ?>
		</select><?php
		} $error = checkPostField("/[0-9]+/i", "trener");
		?>
		<input type="hidden" name="id" value="<?php
			$id = getPostField("id");
			if(is_numeric($id))
				echo $id;
			else if($id)
				View::viewError(ER_CORRUPT_DATA); ?>"/></td>
	</tr><tr>
		<td>Počet hodin (*): </td>
		<td><input type="text" name="pocet_hod" value="<?php echo getPostField("pocet_hod"); ?>" /></td>
		<td>Pouze číslice<?php
			$localError = checkPostField("/^[0-9]*$/i", "pocet_hod");
			$error = $error || $localError;
		?></td>
	</tr><tr>
		<td>Od: (*)</td>
		<td><input type="text" name="od" value="<?php echo getPostField("od"); ?>" /></td>
		<td>Ve formátu např.: 2011-08-14<?php
		//TODO: Date chooser
			$localError = checkPostField("/^[0-9]{4}-[0-9]{2}-[0-9]{2}$/", "od");
			$error = $error || $localError;
		?></td>
	</tr><tr>
		<td>Do: </td>
		<td><input type="text" name="do" value="<?php echo getPostField("do"); ?>" /></td>
		<td>(pokud je prázdný, počítá se jako "Od")<?php
			$localError = strlen(getPostField("do")) < 1 ? 0 :
				checkPostField("/^[0-9]{4}-[0-9]{2}-[0-9]{2}$/", "do");
			$error = $error || $localError;
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