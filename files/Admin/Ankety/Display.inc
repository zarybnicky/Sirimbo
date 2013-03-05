<?php
header_main('Správa anket');
notice(View::getRedirectMessage());
?>
<form action="<?php echo $_SERVER["REQUEST_URI"] ?>" method="POST">
<a href="/admin/ankety/add">Přidat</a>
<button type="submit" name="action" value="edit">Upravit</button>
<button type="submit" name="action" value="remove">Odstranit</button>
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