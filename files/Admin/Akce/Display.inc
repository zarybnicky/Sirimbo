<form action="<?php echo $_SERVER["REQUEST_URI"] ?>" method="POST">
<a href="/admin/akce/add">Přidat</a>
<button type="submit" name="action" value="edit">Upravit</button>
<button type="submit" name="action" value="edit_detail">Upravit přihlášené</button>
<button type="submit" name="action" value="edit_doku">Upravit dokumenty</button>
<button type="submit" name="action" value="remove">Odstranit</button>
<table>
<tr>
<td></td>
<td>Jméno</td>
<td>Místo</td>
<td>Datum</td>
</tr>
<?php
$akce = DBAkce::getAkce();

foreach($akce as $item) {
	echo '<tr>';
	
	echo '<td><input type="checkbox" name="akce[]" value="', $item['a_id'], '" /></td>';
	echo '<td>', $item['a_jmeno'], '</td>';
	echo '<td>', $item['a_kde'], '</td>';
	echo '<td>', formatDate($item['a_od']);
	if($item['a_od'] != $item['a_do'])
		echo ' - ', formatDate($item['a_do']);
	echo '</td>';
	echo '</tr>';
}
?>
</table>
</form>