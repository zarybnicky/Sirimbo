<?php
if(Request::getAction() == 'add')
	header_minor("Přidat nabídku");
else
	header_minor("Upravit nabídku");

if(isset($f))
	notice(implode('<br/>', $f->getMessages()));
?>
<form action="<?php echo Request::getURI();?>" method="POST">
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
		} ?></td>
	</tr><tr>
		<td>Počet hodin (*): </td>
		<td><input type="text" name="pocet_hod" value="<?php echo post("pocet_hod"); ?>" /></td>
	</tr><tr>
		<td>Max. počet hodin/pár (*): </td>
		<td><input type="text" name="max_pocet_hod" value="<?php echo post("max_pocet_hod"); ?>" /></td>
	</tr><tr>
		<td></td>
		<td>(0 = neomezeně)</td>
	</tr><tr>
		<td>Od: (*)</td>
		<td><?php echo Helper::get()->date()->name('od')->textBox();?></td>
	</tr><tr>
		<td>Do: </td>
		<td><?php echo Helper::get()->date()->name('do')->textBox();?></td>
	</tr><tr>
		<td></td>
		<td>(pokud je "Do" prázdné, počítá se jako "Od")</td>
	</tr><tr>
		<td>Viditelný: </td>
		<td><label><?php echo getCheckbox('visible', 'visible', false, false,
			!User::checkPermissionsBool(L_ADMIN));?></label></td>
	</tr><tr>
		<td>Uzamknout: </td>
		<td><label><?php echo getCheckbox('lock');?></label></td>
		<td></td>
	</tr><tr>
		<td><button type="submit"><?php
			if(Request::getAction() == 'add') echo 'Přidat';
			else echo 'Upravit';
			
			echo getReturnInput();
		?></button></td>
		<td>Pole označená (*) jsou povinná</td>
	</tr>
</table>
</form>

<a href="<?php echo Request::getReferer();?>">Zpět</a>