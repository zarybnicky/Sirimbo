<?php
header_main('Správa anket');
notice($this->redirect()->getRedirectMessage());
?>
<form action="" method="POST">
<?php
if(!TISK) {
	echo $this->menu()
		->float(MenuHelper::FLOAT_RIGHT)
		->content('Přidat', 'ankety/add')
		->content('Upravit', 'edit', true)
		->content('Odstranit', 'remove', true)
		->content('Uložit', 'save', true);
}
?>
<table>
<tr>
<td></td>
<td>Jméno</td>
<td>Viditelný</td>
</tr>
<?php
$ankety = DBAnkety::getAnkety();
foreach($ankety as $item) {
	echo '<tr>';
	
	if(Permissions::check('ankety', P_OWNED, $item['ak_kdo']))
		echo '<td><input type="checkbox" name="ankety[]" value="' . $item['ak_id'] .
			'" /></td>';
	else
		echo '<td><input type="checkbox" name="ankety[]" value="' .
			'" disabled="disabled" /></td>';
	echo '<td>', $item['ak_jmeno'], '</td>';
	echo '<td>' . getCheckbox($item['ak_id'], '1', $item['ak_visible']) . '</td>';
	echo '</tr>';
}
?>
</table>
</form>