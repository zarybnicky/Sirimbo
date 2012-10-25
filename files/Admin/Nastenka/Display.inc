<form action="/admin/nastenka" method="POST">
<a href="/admin/nastenka/add">Přidat</a>
<button type="submit" name="action" value="edit">Upravit</button>
<button type="submit" name="action" value="remove">Odstranit</button>
<table>
<tr>
<td></td>
<td>Přidal</td>
<td>Datum</td>
<td>Skupiny</td>
<td>Nadpis</td>
</tr>
<?php
$upozorneni= DBNastenka::getNastenka();
//TODO: Paging, getUpozorneni(OD,DO)
foreach($upozorneni as $item) {
	echo "<tr>";
	if(Permissions::canEditNastenka($item['up_kdo'], $item['up_lock']))
		echo '<td><input type="checkbox" name="nastenka[]" value="' . $item['up_id'] .
			'" /></td>';
	else
		echo '<td><input type="checkbox" name="nastenka[]" value="' .
			'" disabled="d" /></td>';
	echo '<td>';
	echoFullJmeno($item);
	echo '</td>';
	echo '<td>', formatTimestamp($item['up_aktu']), '</td>';
	echo '<td>';
	$skupiny = DBNastenka::getNastenkaSkupiny($item['up_id']);
	foreach($skupiny as $skupina)
		echo '<div class="box" title="', $skupina['ups_popis'], '" ',
			'style="background-color:', Settings::$barvy[$skupina['ups_color']][1], ';"></div>';
	echo '</td>';
	echo '<td>', $item['up_nadpis'], '</td>';
	echo '</tr>';
}
?>
</table>
</form>