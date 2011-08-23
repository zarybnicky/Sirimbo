<?php
$error = false;

global $isConfirmation;
$isConfirmation = (getPostField("nadpis") || getPostField("text"));

if(getPostField("action") == "edit" || getPostField("action") == "edit_confirm")
	$header = "Upravit příspěvek";
else if(getPostField("action") == "add" || getPostField("action") == "add_confirm")
	$header = "Přidat příspěvek";
else
	$header = "Neznámá akce";

header_minor($header);
?>
<form action="<?php echo $_SERVER["REQUEST_URI"] ?>" method="POST">
<table>
	<tr>
		<td>Nadpis (*): </td>
		<td><input type="text" name="nadpis" value="<?php echo getPostField("nadpis"); ?>" />
			<input type="hidden" name="id" value="<?php $id = getPostField("id");
			if(is_numeric($id)) echo $id; else if($id) View::viewError(ER_CORRUPT_DATA); ?>"/></td>
		<td></td>
	</tr><tr>
		<td>Text (*): </td>
		<td><textarea cols="50" rows="15" name="text"><?php echo getPostField("text"); ?></textarea></td>
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