<?php
header_minor('Moje platby');

$skupina = User::getSkupinaData();
echo '<div class="box" title="', $skupina['us_popis'], '" ',
	'style="background-color:', Settings::$barvy[$skupina['us_color']][1] . ';"></div>&nbsp;',
	$skupina['us_popis'], '<br/>';
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
		echo '<td><div class="box" title="', $platba['us_popis'], '" ',
			'style="background-color:', Settings::$barvy[$platba['us_color']][1] . ';"></div></td>';
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
	
	echo '<tr><td><div class="box" title="', $skupina['us_popis'], '" ',
		'style="background-color:', Settings::$barvy[$skupina['us_color']][1] . ';"></div>&nbsp;',
		$skupina['us_popis'], '</td>',
		'<td>', $skupina['us_platba_ctvrtrok'], '</td>',
		'<td>', $skupina['us_platba_pulrok'], '</td></tr>';
}
echo '</table>';

echo '<a href="/member/profil">Zpět</a>'
?>