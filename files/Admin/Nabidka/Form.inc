<?php
$error = false;

global $isConfirmation;
$isConfirmation = (getPostField("trener") || getPostField("pocet_hod"));

if(getPostField("action") == "edit" || getPostField("action") == "edit_confirm")
	$header = "Upravit nabídku";
else if(getPostField("action") == "add" || getPostField("action") == "add_confirm")
	$header = "Přidat nabídku";
else
	$header = "Neznámá akce";

header_minor($header);
?>
<form action="<?php echo $_SERVER["REQUEST_URI"] ?>" method="POST">
<table>
	<tr>
		<td>Trenér (*): </td>
		<td><?php
		if(!User::checkPermissionsBool(L_ADMIN)) { ?>
		<select name="trener">
			<option value="<?php echo User::getUserID();?>">Já</option>
		</select><?php
		} else { ?>
		<select name="trener">
			<option value="none" <?php if(!getPostField("trener")) echo "selected=\"selected\"";?>>
				Vyber si :o)</option>
			<?php
			$treneri = DBUser::getTrener();
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
			$error = checkPostField("/^[0-9]+$/i", "pocet_hod") || $error;
		?></td>
	</tr><tr>
		<td>Od: (*)</td>
		<!--<td><input type="text" name="od" value="<?php //echo getPostField("od"); ?>" /></td>-->
		<!--$error = checkPostField("/^[0-9]{4}-[0-9]{2}-[0-9]{2}$/", "od") || $error;-->
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
		<td>Viditelný: </td>
		<td><input type="checkbox" name="visible" value="visible" <?php
			if(getPostField("visible")) echo "checked=\"checked\"";
			if(!User::checkPermissionsBool(L_ADMIN)) echo "readonly=\"readonly\""
		?>/></td>
		<td></td>
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