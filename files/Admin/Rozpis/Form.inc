<?php
header_main('Správa rozpisů');
if(Request::getAction() == 'add')
	header_minor("Přidat rozpis");
else
	header_minor("Upravit rozpis");

notice(View::getRedirectMessage());
if(isset($f))
	notice(implode('<br/>', $f->getMessages()));
?>
<form action="<?php echo Request::getURI();?>" method="POST">
<table>
	<tr>
		<td>Trenér (*): </td>
		<td><?php
			if(!Permissions::check('rozpis', P_ADMIN)) { ?>
			<select name="trener">
				<option value="<?php echo User::getUserID();?>">Já</option>
			</select><?php
			} else {
				echo Helper::get()->userSelect()
					->name('trener')
					->users(DBUser::getUsersByPermission('rozpis', P_OWNED));
			}
			?>
		</td>
	</tr><tr>
		<td>Místo (*): </td>
		<td><input type="text" name="kde" value="<?php echo post("kde"); ?>" /></td>
		<td></td>
	</tr><tr>
		<td>Datum: (*)</td>
		<td><?php echo Helper::get()->date()->name('datum')->textBox();?></td>
	</tr><tr>
		<td>Viditelný: </td>
		<td><label><?php echo getCheckbox('visible', 'visible', false, false,	
			!Permissions::check('rozpis', P_ADMIN));?></label></td>
	</tr><tr>
		<td>Uzamknout: </td>
		<td><label><?php echo getCheckbox('lock', 'lock')?></label></td>
	</tr><tr>
		<td><button type="submit"><?php
			if(Request::getAction() == 'add') echo 'Přidat';
			else echo 'Upravit';
		?></button></td>
		<td>Pole označená (*) jsou povinná</td>
	</tr>
</table>
</form>
<a href="/admin/rozpis">Zpět</a>