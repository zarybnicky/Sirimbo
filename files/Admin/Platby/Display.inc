<?php
if(!TISK) {
	$s = new SelectHelper();
	
	echo '<form action="/admin/platby" method="get">';
	echo 'za období:&nbsp;';
	$s->select()
		->name('f')
		->option('all', 'Všechny platby');
	$year = date('Y');
	for($i = '2011'; $i <= $year; $i++) {
		$s->option($i . '-1' , '1. pololetí ' . $i . '/' . ($i + 1));
		$s->option($i . '-2' , '2. pololetí ' . $i . '/' . ($i + 1));
	}
	echo $s->value($year . '-1');
	
	echo 'řadit podle:&nbsp;';
	echo $s->select()
		->name('s')
		->option('surname', 'příjmení')
		->option('var-symbol', 'var. symbolu')
		->option('datum', 'data platby');
	echo '<button type="submit">Odeslat</button><br/><br/>';
	echo '</form>';
}

echo '<form action="/admin/platby" method="POST">';
if(!TISK) {
	$f = new MenuHelper();
	echo $f->menu()
		->float(MenuHelper::FLOAT_RIGHT)
		->content('Přidat', 'platby/add')
		->content('Upravit', 'edit', true)
		->content('Odebrat', 'remove', true);
}

$skupiny = DBSkupiny::getSkupiny();

if(!get('f') || get('f') == 'all') {
	$platby = DBPlatby::getPlatby();
} else {
	list($year, $n) = explode('-', get('f'));
	
	if($n == 1) {
		$od = $year . Settings::$platby_obdobi['1-pololeti'][0];
		$do = ((int) $year + 1) . Settings::$platby_obdobi['1-pololeti'][1];
	} elseif($n == 2) {
		$od = ((int) $year + 1) . Settings::$platby_obdobi['2-pololeti'][0];
		$do = ((int) $year + 1) . Settings::$platby_obdobi['2-pololeti'][1];
	}
	$platby = DBPlatby::getPlatbyByDate($od, $do);
}
if(empty($platby)) {
	notice('Žádné platby za toto období');
	return;
}
$sort = get('s') ? get('s') : 'surname';
switch($sort) {
	case 'var-symbol':
		usort($platby, create_function('$a, $b', 'return $a["u_id"] > $b["u_id"];'));
		break;
	case 'surname':
		usort($platby, create_function('$a, $b', 'return $a["u_prijmeni"] > $b["u_prijmeni"];'));
		break;
	case 'datum':
		usort($platby, create_function('$a, $b', 'return $a["up_placeno"] < $b["up_placeno"];'));
		break;
	default:
		break;
}

echo '<table>';
echo '<tr>';
echo '<td></td>';
echo '<td>Jméno</td>';
echo '<td></td>';
echo '<td>Variabilní<br/>symbol</td>';
echo '<td>Období</td>';
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
	echo '<td>', User::var_symbol($platba['u_id']), '</td>';
	echo '<td>', $platba['up_obdobi'] == true ?
		Settings::$platby_obdobi[$platba['up_obdobi']][3] : '', '</td>';
	echo '<td>';
	if((strpos($platba['up_obdobi'], 'pololeti') &&
			$platba['up_castka'] != $platba['us_platba_pulrok']) ||
		(strpos($platba['up_obdobi'], 'ctvrtleti') &&
			$platba['up_castka'] != $platba['us_platba_ctvrtrok']))
		echo '<span style="color:black;font-weight:bold;">',
			$platba['up_castka'], ' Kč</span>';
	else
		echo '<span style="color:#0B0;">',
			$platba['up_castka'], ' Kč</span>';
	echo '</td>';
	echo '<td>', formatDate($platba['up_placeno']), '</td>';
	echo '<td>', formatDate($platba['up_plati_do']), '</td>';
	echo '</tr>';
}
echo '</table>';
echo '</form>';
?>