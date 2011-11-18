<form action="<?php echo $_SERVER["REQUEST_URI"] ?>" method="POST">
<a href="/admin/nabidka/add">Přidat</a>
<button type="submit" name="action" value="edit">Upravit</button>
<button type="submit" name="action" value="edit_detail">Upravit tréninky</button>
<button type="submit" name="action" value="remove">Odstranit</button>
<table>
<tr>
<td></td>
<td>Trenér</td>
<td>Datum</td>
<td>Viditelný</td>
</tr>
<?php
$nabidka = DBNabidka::getNabidka();

foreach($nabidka as $item) {
	echo '<tr>';
	
	if(Permissions::canEditNabidka($item['n_trener']))
		echo '<td><input type="checkbox" name="nabidka[]" value="' . $item['n_id'] .
			'" /></td>';
	else
		echo '<td><input type="checkbox" name="nabidka[]" value="' .
			'" disabled="disabled" /></td>';
	echo '<td>';
	echoFullJmeno($item);
	echo '</td>';
	echo '<td>' . formatDate($item['n_od']) . ' - ' . formatDate($item['n_do']) .  '</td>';
	echo '<td>' . ($item['n_visible'] ? 'x' : '_') . '</td>';
	echo '</tr>';
}
?>
</table>
</form>