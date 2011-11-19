<form action="<?php echo $_SERVER["REQUEST_URI"] ?>" method="POST">
<a href="/admin/inzerce/add">Přidat</a>
<button type="submit" name="action" value="edit">Upravit</button>
<button type="submit" name="action" value="remove">Odstranit</button>
<a href="/admin/inzerce/new">Nepotvrzené inzeráty</a>
<table>
<tr>
<td></td>
<td>Nadpis</td>
<td>Jméno</td>
<td>Kategorie</td>
<td>Viditelný</td>
</tr>
<?php
$inzerce = DBInzerce::getInzerce(false, true);

foreach($inzerce as $item) {
	echo '<tr>';
	echo '<td><input type="checkbox" name="inzerce[]" value="' . $item['i_id'] .
		'" /></td>';
	echo '<td>', $item['i_jmeno'], ' ', $item['i_prijmeni'], '</td>';
	echo '<td>' . formatDate($item['i_od']) . ' - ' . formatDate($item['i_do']) .  '</td>';
	echo '<td>';
	switch($item['i_kat']) {
		case INZERCE_KOUPIM: echo 'Koupím'; break;
		case INZERCE_PRODAM: echo 'Prodám'; break;
		case INZERCE_PARTNER: echo 'Hledám partnera'; break;
		case INZERCE_PARTNERKA: echo 'Hledám partnerku'; break;
	}
	echo '</td>';
	echo '<td>' . ($item['i_visible'] ? 'x' : '_') . '</td>';
	echo '</tr>';
}
?>
</table>
</form>