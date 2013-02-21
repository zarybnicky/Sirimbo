<?php
header_main('Správa plateb');
notice(View::getRedirectMessage());

if(!TISK) {
	$s = Helper::get()->select();
	
	echo '<form action="/admin/platby" method="get">';
	echo 'za období:&nbsp;';
	$s->get()->name('f')
		->option('all', 'Všechny platby', true);
	$year = date('Y');
	for($i = '2011'; $i <= $year; $i++) {
		$s->option($i . '-1' , '1. pololetí ' . $i . '/' . ($i + 1));
		$s->option($i . '-2' , '2. pololetí ' . $i . '/' . ($i + 1));
	}
	echo $s;
	
	echo 'řadit podle:&nbsp;';
	echo $s->get()->name('s')
		->option('prijmeni', 'příjmení', true)
		->option('var-symbol', 'var. symbolu')
		->option('placeno', 'data platby');
	echo '<button type="submit">Odeslat</button><br/><br/>';
	echo '</form>';
}

$skupiny = DBSkupiny::getSkupiny();
if(!get('f') || get('f') == 'all') {
	$options['type'] = 'platby';
} else {
	list($year, $n) = explode('-', get('f'));
	
	if($n == 1) {
		$od = $year . Settings::$platby_obdobi['1-pololeti'][0];
		$do = ((int) $year + 1) . Settings::$platby_obdobi['1-pololeti'][1];
	} elseif($n == 2) {
		$od = ((int) $year + 1) . Settings::$platby_obdobi['2-pololeti'][0];
		$do = ((int) $year + 1) . Settings::$platby_obdobi['2-pololeti'][1];
	}
	$options = array('type' => 'date', 'od' => $od, 'do' => $do);
}

$options['sort'] = in_array(get('s'), array('prijmeni', 'placeno', 'var-symbol')) ?
	get('s') : 'prijmeni';
$pager = new Paging(new PagingAdapterDBSelect('DBPlatby', $options));
$pager->setCurrentPageField('p');
$pager->setItemsPerPageField('c');
$pager->setDefaultItemsPerPage(20);
$pager->setPageRange(5);
$items = $pager->getItems();

echo '<form action="', Request::getURI(), '" method="POST">';
if(!TISK) {
	echo Helper::get()->menu()
		->float(MenuHelper::FLOAT_RIGHT)
		->content('Přidat', 'platby/add')
		->content('Upravit', 'edit', true)
		->content('Odebrat', 'remove', true);
}
if(empty($items)) {
	notice('Žádné platby za toto období');
	return;
}

echo '<div style="margin-right:150px">';
echo '<table style="width:100%">';
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

foreach($items as $item) {
	echo "<tr>";
	echo '<td><input type="checkbox" name="platby[]" value="' . $item['up_id'] .	'" /></td>';
	echo '<td>', $item['u_prijmeni'], ', ', $item['u_jmeno'], '</td>';
	echo '<td>', getColorBox($item['us_color'], $item['us_popis']), '</td>';
	echo '<td>', User::var_symbol($item['u_id']), '</td>';
	echo '<td>', $item['up_obdobi'] == true ?
		Settings::$platby_obdobi[$item['up_obdobi']][3] : '', '</td>';
	echo '<td>';
	if((strpos($item['up_obdobi'], 'pololeti') &&
			$item['up_castka'] != $item['us_platba_pulrok']) ||
		(strpos($item['up_obdobi'], 'ctvrtleti') &&
			$item['up_castka'] != $item['us_platba_ctvrtrok']))
		echo '<span style="color:black;font-weight:bold;">',
			$item['up_castka'], ' Kč</span>';
	else
		echo '<span style="color:#0B0;">',
			$item['up_castka'], ' Kč</span>';
	echo '</td>';
	echo '<td>', formatDate($item['up_placeno']), '</td>';
	echo '<td>', formatDate($item['up_plati_do']), '</td>';
	echo '</tr>';
}
echo '</table>';
echo '</div>';
echo '</form>';
echo '<div style="margin-right:15Opx;text-align:center;">';
echo $pager->getNavigation();
echo '</div>';
?>