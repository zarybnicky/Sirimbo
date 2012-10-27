<?php
$platby = DBPlatby::getPlatbyFromUser($id);
$skupiny = DBSkupiny::getSkupiny();

header_minor($data['u_prijmeni'] . ', ' . $data['u_jmeno']);

echo '<table>';
echo '<tr>';
echo '<td>Skupina</td>';
echo '<td>Kč/měsíc</td>';
echo '<td>Částka</td>';
echo '<td>Placeno</td>';
echo '<td>Platí do</td>';
echo '<td></td>';
echo '</tr>';

foreach($platby as $platba) {
	echo "<tr>";
	echo '<td><div class="box" title="', $platba['us_popis'], '" ',
		'style="background-color:', Settings::$barvy[$platba['us_color']][1] . ';"></div></td>';
	echo '<td>', $platba['us_platba_mesic'], ' Kč</td>';
	echo '<td>', $platba['up_castka'], ' Kč</td>';
	echo '<td>', formatDate($platba['up_placeno']), '</td>';
	echo '<td>', formatDate($platba['up_plati_do']), '</td>';
	echo '<td><a href="/admin/platby/edit/', $platba['up_id'], '">Upravit</a></td>';
	echo '</tr>';
}
echo '</table>';

echo '<a href="/admin/users">Zpět</a>';
?>