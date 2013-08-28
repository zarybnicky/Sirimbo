<?php
header_main('Správa anket');
if(Request::getAction() == 'add')
	header_minor("Přidat anketu");
else
	header_minor("Upravit anketu");

notice($this->redirect()->getRedirectMessage());
?>
<form action="" method="post">
<button type="submit" name="action" value="enter" style="display:none;">Enter</button>
<table>
	<tr>
		<td>Jméno: </td>
		<td><input type="text" name="jmeno" value="<?php echo post('jmeno');?>" /></td>
	</tr>
	<tr>
		<td>Text: </td>
		<td><input type="text" name="text" value="<?php echo post('text');?>" /></td>
	</tr><tr>
		<td>Viditelný: </td>
		<td><label><?php echo getCheckbox('visible', 'visible', false, false,
			!Permissions::check('ankety', P_OWNED));?></label>
		</td>
	</tr><tr>
		<td>Text</td><td>Počet hlasů</td>
	</tr>
	<?php
	if(Request::getAction() == 'edit' && ($items = DBAnkety::getAnketaItems(Request::getID()))) {
		foreach($items as $item) {
				echo '<tr>';
				echo '<td><input type="text" name="', $item['aki_id'], '-text" value="', $item['aki_text'],
					'"/></td>';
				echo '<td>', isset($item['aki_pocet']) ? (int) $item['aki_pocet'] : 0, '</td>';
				echo '<td><button type="submit" name="remove" value="', $item['aki_id'], '">',
					'Odstranit a uložit</button></td>';
				echo '</tr>';
		}
	}
	?>
	<tr>
		<td><input type="text" name="add_text" /></td>
		<td>0</td>
		<td><button type="submit" name="action" value="confirm">Přidat a uložit</button></td>
	</tr><tr>
		<td><button type="submit" name="action" value="confirm">Uložit</button></td>
		<td><a href="/admin/ankety">Zpět</a></td>
	</tr>
</table>
</form>