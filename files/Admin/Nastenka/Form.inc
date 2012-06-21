<?php
$error = false;

global $isConfirmation;
$isConfirmation = (post("nadpis") || post("text"));

$request = explode('/', $_SERVER['REQUEST_URI']);
$action = array_slice($request, 3);

if($action[0] == 'add')
	$header = "Přidat příspěvek";
elseif($action[0] == 'edit')
	$header = "Upravit příspěvek";
else
	$header = "Neznámá akce";

header_minor($header);
?>
<form action="/admin/nastenka" method="POST">
<table>
	<tr>
		<td>Nadpis: </td>
		<td><input type="text" name="nadpis" value="<?php echo post("nadpis"); ?>" />
			<input type="hidden" name="id" value="<?php $id = post("id");
			if(is_numeric($id)) echo $id; else if($id) View::viewError(ER_CORRUPT_DATA); ?>"/></td>
		<td></td>
	</tr><tr>
		<td>Text: </td>
		<td><textarea cols="50" rows="15" name="text"><?php echo post("text"); ?></textarea></td>
		<td></td>
	</tr><tr>
		<td>Uzamknout: </td>
		<td><input type="checkbox" name="lock" value="lock" <?php
			if(post("lock")) echo "checked=\"checked\"";
		?>/></td>
		<td></td>
	</tr><tr>
		<td>Barvy: </td>
		<td><input type="checkbox" name="zluta" value="zluta" <?php
			if(post("zluta")) echo "checked=\"checked\"";
		?>/>Žlutá, <input type="checkbox" name="cervena" value="cervena" <?php
			if(post("cervena")) echo "checked=\"checked\"";
		?>/>Červená, <input type="checkbox" name="modra" value="modra" <?php
			if(post("modra")) echo "checked=\"checked\"";
		?>/>Modrá</td>
		<td></td>
	</tr><tr>
		<?php if($action[0] == 'add') {?>
		<td><button type="submit" name="action" value="add_confirm">Přidat</button></td>
		<?php } else {?>
		<td><button type="submit" name="action" value="edit_confirm">Upravit</button></td>
		<?php }	?>
		<td></td>
		<td>Pole označená (*) jsou povinná</td>
	</tr>
</table>
</form>

<a href="/admin/nastenka">Zpět</a>