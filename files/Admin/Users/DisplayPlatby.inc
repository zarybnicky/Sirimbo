<?php
header_main('Správa uživatelů');
header_minor('Platby uživatele ' . $data['u_jmeno'] . ' ' . $data['u_prijmeni']);
notice(View::getRedirectMessage());

echo Helper::get()->menu()
	->float(MenuHelper::FLOAT_RIGHT)
	->content('Přidat', '/admin/platby/add?u=' . $data['u_id'])
	->content('Zpět', '/admin/users');

echo '<table>';
echo '<tr>';
echo '<td>Skupina</td>';
echo '<td>Kč/měsíc</td>';
echo '<td>Částka</td>';
echo '<td>Placeno</td>';
echo '<td>Platí do</td>';
echo '<td></td>';
echo '</tr>';

$platby = DBPlatby::getPlatbyFromUser($id);
foreach($platby as $platba) {
	echo "<tr>";
	echo '<td>', getColorBox($platba['us_color'], $platba['us_popis']), '</td>';
	echo '<td>', $platba['us_platba_mesic'], ' Kč</td>';
	echo '<td>', $platba['up_castka'], ' Kč</td>';
	echo '<td>', formatDate($platba['up_placeno']), '</td>';
	echo '<td>', formatDate($platba['up_plati_do']), '</td>';
	echo '<td><a href="/admin/platby/edit/', $platba['up_id'], '">Upravit</a></td>';
	echo '</tr>';
}
echo '</table>';
?>