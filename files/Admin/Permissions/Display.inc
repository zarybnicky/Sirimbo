<?php
echo '<form action="/admin/permissions" method="POST">';
if(!TISK) {
	echo Helper::get()->menu()
		->float(MenuHelper::FLOAT_RIGHT)
		->content('Přidat', 'permissions/add')
		->content('Upravit', 'edit', true)
		->content('Odebrat', 'remove', true);
}
echo '<div style="margin-right:150px">';
echo '<table style="width:100%">';
echo '<tr>';
echo '<td></td>';
echo '<td>Jméno</td>';
echo '<td>Popis</td>';
echo '</tr>';

$data = DBPermissions::getGroups();
foreach($data as $item) {
	echo "<tr>";
	echo '<td><input type="checkbox" name="permissions[]" value="' . $item['pe_id'] .	'" /></td>';
	echo '<td>', $item['pe_name'], '</td>';
	echo '<td>', $item['pe_description'], '</td>';
	echo '</tr>';
}
echo '</table>';
echo '</div>';
echo '</form>';
?>