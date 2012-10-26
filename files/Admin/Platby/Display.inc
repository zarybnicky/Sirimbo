<?php
echo '<form action="/admin/platby" method="POST">';

if(!TISK) {
	$f = new MenuHelper();
	echo $f->menu()
		->float(MenuHelper::FLOAT_RIGHT)
		->content('Přidat', 'platby/add')
		->content('Upravit', 'edit', true)
		->content('Odebrat', 'remove', true);
}

$platby = DBPlatby::getPlatby();
$skupiny = DBSkupiny::getSkupiny();

echo '<table>';
echo '<tr>';
echo '<td></td>';
echo '<td>Jméno</td>';
echo '<td>Skupina</td>';
echo '<td>Kč/měsíc</td>';
echo '<td>Částka</td>';
echo '<td>Placeno</td>';
echo '<td>Platí do</td>';
echo '</tr>';

foreach($platby as $platba) {
	echo "<tr>";
	echo '<td><input type="checkbox" name="platby[]" value="' . $platba['up_id'] .	'" /></td>';
	echo '<td>', $platba['u_prijmeni'], ', ', $platba['u_jmeno'], '</td>';
	echo '<td><div class="box" title="', $platba['us_popis'], '" ',
		'style="background-color:', Settings::$barvy[$platba['us_color']][1] . ';"></div></td>';
	echo '<td>', $platba['us_platba_mesic'], ' Kč</td>';
	echo '<td>', $platba['up_castka'], ' Kč</td>';
	echo '<td>', formatDate($platba['up_placeno']), '</td>';
	echo '<td>', formatDate($platba['up_plati_do']), '</td>';
	echo '</tr>';
}
echo '</table>';
echo '</form>';
?>