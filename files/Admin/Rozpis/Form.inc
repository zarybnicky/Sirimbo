<?php
$error = false;

global $isConfirmation;
$isConfirmation = (post("trener") || post("kde"));

if(Request::getAction() == 'edit')
	$header = "Upravit rozpis";
elseif(Request::getAction() == 'add')
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
			echo Helper::get()->userSelect()
				->name('trener')
				->permissions(L_TRENER)
				->users(DBUser::getTrener());
		}
		$error = checkPostField("/[0-9]+/i", "trener");
		?>
		<input type="hidden" name="id" value="<?php $id = post("id");
			if(is_numeric($id)) echo $id; else if($id) View::viewError(ER_CORRUPT_DATA); ?>"/></td>
	</tr><tr>
		<td>Místo (*): </td>
		<td><input type="text" name="kde" value="<?php echo post("kde"); ?>" /></td>
		<td></td>
	</tr><tr>
		<td>Datum: (*)</td>
		<td><?php echo Helper::get()->date()->name('datum')->textBox();?></td>
		<td><?php $error = checkDateString(Helper::get()->date(false)->getPost()) || $error;?></td>
	</tr><tr>
		<td>Viditelný: </td>
		<td><label><?php echo getCheckbox('visible', 'visible', false, false,	
			!User::checkPermissionsBool(L_ADMIN));?></label></td>
		<td></td>
	</tr><tr>
		<td>Uzamknout: </td>
		<td><label><?php echo getCheckbox('lock', 'lock')?></label></td>
		<td></td>
	</tr><tr>
		<?php if(Request::getAction() == 'add') {?>
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