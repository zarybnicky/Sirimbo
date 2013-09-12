<?php
header_main('Správa rozpisů');
if($this->action == 'add')
	header_minor("Přidat rozpis");
else
	header_minor("Upravit rozpis");
notice($this->redirect()->getMessage());
?>
<form action="" method="POST">
<table>
	<tr>
		<td>Trenér (*): </td>
		<td><?php
			if(!$this->isAdmin) { ?>
			<select name="trener">
				<option value="<?php echo User::getUserID();?>">Já</option>
			</select><?php
			} else {
				echo $this->userSelect()
					->name('trener')
					->users(DBUser::getUsersByPermission('rozpis', P_OWNED));
			}?>
		</td>
	</tr><tr>
		<td>Místo (*): </td>
		<td><input type="text" name="kde" value="<?php echo post("kde"); ?>" /></td>
	</tr><tr>
		<td>Datum: (*)</td>
		<td><?php echo $this->date('datum')->textBox();?></td>
	</tr><tr>
		<td>Viditelný: </td>
		<td><label><?php
			if($this->isAdmin)
				echo getCheckbox('visible', 'visible', false);
			else
				echo (post('visible') ? '&#10003;' : '&#10799;');
		?></label></td>
	</tr><tr>
		<td>Uzamknout: </td>
		<td><label><?php echo getCheckbox('lock', 'lock')?></label></td>
	</tr><tr>
		<td><button type="submit"><?php
			echo ($this->action == 'add' ? 'Přidat' : 'Upravit');
		?></button></td>
		<td>Pole označená (*) jsou povinná</td>
	</tr>
</table>
</form>
<a href="/admin/rozpis">Zpět</a>