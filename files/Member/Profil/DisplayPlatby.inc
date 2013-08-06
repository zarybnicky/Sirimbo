<?php
header_minor('Moje platby');

$skupina = User::getSkupinaData();
echo getColorBox($skupina['us_color'], $skupina['us_popis']),
	'&nbsp;', $skupina['us_popis'], '<br/>';
echo 'Variabilní symbol: ', User::var_symbol(User::getUserID()), '<br/><br/>';

echo 'Aktuálně: ', User::getZaplaceno() ? 'zaplaceno' : 'NEZAPLACENO!',
	'<br/><br/>';

$platby = DBPlatby::getPlatbyFromUser(User::getUserID());
if(!empty($platby)) {
	header_minor('Historie plateb');
	
	echo '<table>';
	echo '<tr>';
	echo '<td>Skupina</td>';
	echo '<td>Období</td>';
	echo '<td>Částka</td>';
	echo '<td>Placeno</td>';
	echo '<td>Platí do</td>';
	echo '</tr>';
	
	foreach($platby as $platba) {
		echo "<tr>";
		echo '<td>', getColorBox($platba['us_color'], $platba['us_popis']), '</td>';
		echo '<td>', Settings::$platby_obdobi[$platba['up_obdobi']][3], ' Kč</td>';
		echo '<td>', $platba['up_castka'], ' Kč</td>';
		echo '<td>', formatDate($platba['up_placeno']), '</td>';
		echo '<td>', formatDate($platba['up_plati_do']), '</td>';
		echo '</tr>';
	}
	echo '</table><br/>';
}

header_minor('Přehled skupin');
$skupiny = DBSkupiny::getSkupiny();
echo '<table>';
echo '<tr><td>Skupina</td>',
	'<td>Kč/čtvrtrok</td>',
	'<td>Kč/půlrok</td></tr>';
foreach($skupiny as $skupina) {
	if($skupina['us_platba_mesic'] == 0 && $skupina['us_platba_mesic'] == 0 &&
			$skupina['us_platba_mesic'] == 0)
		continue;

	echo '<tr>';
	echo '<td>', getColorBox($skupina['us_color'], $skupina['us_popis']), '</td>',
		'&nbsp;', $skupina['us_popis'], '</td>',
		'<td>', $skupina['us_platba_ctvrtrok'], '</td>',
		'<td>', $skupina['us_platba_pulrok'], '</td>';
	echo '</tr>';
}
echo '</table>';

echo '<a href="/member/profil">Zpět</a>'
?>