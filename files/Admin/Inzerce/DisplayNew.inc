<?php
header_main('Správa inzerce');
notice(View::getRedirectMessage());
?>
<form action="<?php echo $_SERVER["REQUEST_URI"]?>" method="POST">
<button type="submit" name="action" value="confirm">Potvrdit</button>
<button type="submit" name="action" value="remove">Odstranit</button>
<table>
<tr>
<td></td>
<td>Viditelný</td>
<td>Nadpis</td>
<td>Jméno</td>
<td>Text</td>
<td>Kategorie</td>
</tr>
<?php
$inzerce = DBInzerce::getInzerce(false, false);
foreach($inzerce as $data) {
	echo '<tr>';
	echo '<td><input type="checkbox" name="inzerce[]" value="' . $data['i_id'] . '" /></td>';
	echo '<td><input type="checkbox" name="', $data['i_id'], '-visible" value="' . $data['i_id'] . '" /></td>';
	echo '<td>', $data['i_nadpis'], '</td>';
	echo '<td>', $data['i_jmeno'], ' ', $data['i_prijmeni'], '</td>';
	echo '<td>', $data['i_text'], '</td>';
	echo '<td>';
	switch($data['i_kat']) {
		case INZERCE_KOUPIM: echo 'Koupím'; break;
		case INZERCE_PRODAM: echo 'Prodám'; break;
		case INZERCE_PARTNER: echo 'Hledám partnera'; break;
		case INZERCE_PARTNERKA: echo 'Hledám partnerku'; break;
	}
	echo '</td>';
	echo '</tr>';
}
?>
</table>
</form>

<a href="/admin/inzerce">Zpět</a>