<?php
$error = false;

global $isConfirmation;
$isConfirmation = (getPostField("trener") || getPostField("kde"));

$request = explode('/', $_SERVER['REQUEST_URI']);
$action = array_slice($request, 3);
if(empty($action)) $action[] = '';

if($action[0] == 'edit')
	$header = "Upravit rozpis";
elseif($action[0] == 'add')
	$header = "Přidat rozpis";
else
	$header = "Neznámá akce";
	
header_minor($header);
?>
<form action="<?php echo $_SERVER['REQUEST_URI']; ?>" method="POST">
<table>
	<tr>
		<td>Trenér (*): </td>
		<td><?php
		if(!User::checkPermissionsBool(L_ADMIN)) { ?>
		<select name="trener">
			<option value="<?php echo User::getUserID();?>">Já</option>
		</select><?php
		} else {
			$treneri = DBUser::getTrener();
			echoUsers("trener", $treneri);
		}
		$error = checkPostField("/[0-9]+/i", "trener");
		?>
		<input type="hidden" name="id" value="<?php $id = getPostField("id");
			if(is_numeric($id)) echo $id; else if($id) View::viewError(ER_CORRUPT_DATA); ?>"/></td>
	</tr><tr>
		<td>Místo (*): </td>
		<td><input type="text" name="kde" value="<?php echo getPostField("kde"); ?>" /></td>
		<td></td>
	</tr><tr>
		<td>Datum: (*)</td>
		<td><?php echoDateSelect("day", "month", "year");?></td>
		<td><?php
			$od_str = getPostField("year") . "-" . getPostField("month") . "-" .
				getPostField("day");
			$error = checkDateString($od_str) || $error;
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
		<?php if($action[0] == 'add') {?>
		<td><button type="submit" name="action" value="add_confirm">Přidat</button></td>
		<?php }	else {?>
		<td><button type="submit" name="action" value="edit_confirm">Upravit</button></td>
		<?php } ?>
		<td></td>
		<td>Pole označená (*) jsou povinná</td>
	</tr>
</table>
</form>
<a href="/admin/rozpis">Zpět</a>