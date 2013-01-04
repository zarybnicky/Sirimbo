<?php
if(get('v') === null)
	get('v', 'info');

if(!TISK) {
	echo '<form action="', Request::getURI(), '" method="get">';

	echo 'zobrazení:&nbsp;';
	echo Helper::get()->select()
		->get()->name('v')
		->option('info', 'skupiny')
		->option('users', 'uživatelé podle skupin');
	echo '<button type="submit">Odeslat</button>';
	echo '</form><br/>';
}

$skupiny = DBSkupiny::getSkupiny();
if(get('v') == 'info') {
	echo '<form action="/admin/skupiny" method="POST">';
	
	if(!TISK) {
		echo Helper::get()->menu()
			->float(MenuHelper::FLOAT_RIGHT)
			->content('Přidat', 'skupiny/add')
			->content('Upravit', 'edit', true)
			->content('Odebrat', 'remove', true);
	}
	echo '<div style="margin-right:150px">';
	echo '<table style="width:100%">';
	echo '<tr>';
	echo '<td></td>';
	echo '<td>Barvy</td>';
	echo '<td>Popis</td>';
	echo '<td>Kč/měsíc</td>';
	echo '<td>Kč/čtvrtrok</td>';
	echo '<td>Kč/půlrok</td>';
	echo '</tr>';
	
	foreach($skupiny as $item) {
		echo "<tr>";
		echo '<td><input type="checkbox" name="skupiny[]" value="' . $item['us_id'] .	'" /></td>';
		echo '<td>', getColorBox($item['us_color'], $item['us_popis']), '</td>';
		echo '<td style="max-width:300px;">', $item['us_popis'], '</td>';
		echo '<td class="center">', $item['us_platba_mesic'], ' Kč</td>';
		echo '<td class="center">', $item['us_platba_ctvrtrok'], ' Kč</td>';
		echo '<td class="center">', $item['us_platba_pulrok'], ' Kč</td>';
		echo '</tr>';
	}
	echo '</table>';
	echo '</div>';
	echo '</form>';
} elseif(get('v') == 'users') {
	foreach($skupiny as $key => &$skupina) {
		if(!$skupina['us_platba_mesic'] &&
			!$skupina['us_platba_ctvrtrok'] && !$skupina['us_platba_pulrok']) {
			unset($skupiny[$key]);
			continue;
		}
		$skupina['users'] = DBUser::getUsersBySkupina($skupina['us_id']);
	}
	unset($skupina);
	function sortByLength($arr1, $arr2) {
		$c1 = count($arr1['users']);$c2 = count($arr2['users']);
		return $c1 > $c2 ? -1 : $c1 == $c2 ? 0 : 1;
	}
	usort($skupiny, 'sortByLength');
	
	$content = array(array(), array());
	$count = array(0, 0);
	foreach($skupiny as $skupina) {
		$min = 0;
		foreach($count as $k => $i)
			if($i < $count[$min]) $min = $k;
		$content[$min][] = $skupina;
		$count[$min] += count($skupina['users']);
	}
	echo '<table style="width:100%;border:none;background:inherit;">';
	echo '<tr style="border:none;background:inherit;">';
	foreach($content as $row) {
		echo '<td style="border:none;background:inherit;vertical-align:top;">';
		foreach($row as $skupina) {
			header_minor(getColorBox($skupina['us_color'], $skupina['us_popis']) .
				'&nbsp;&nbsp;' . $skupina['us_popis']);
			
			$users = DBUser::getUsersBySkupina($skupina['us_id']);
			if(empty($users)) {
				echo '<br/>';
				continue;
			}
			
			echo '<div>';
			echo '<table style="width:100%">';
			echo '<tr>';
			echo '<td>Jméno</td>';
			echo '<td>Částka</td>';
			echo '<td>Zaplaceno do</td>';
			echo '</tr>';
			foreach($users as $user) {
				echo '<tr>';
				echo '<td style="width:160px;">', $user['u_prijmeni'], ', ', $user['u_jmeno'], '</td>';
	
				echo '<td style="width:75px;">';
				if($user['up_id']) {
					if((strpos($user['up_obdobi'], 'pololeti') &&
							$user['up_castka'] != $user['us_platba_pulrok']) ||
						(strpos($user['up_obdobi'], 'ctvrtleti') &&
							$user['up_castka'] != $user['us_platba_ctvrtrok']))
						echo '<span style="color:black;font-weight:bold;">',
							$user['up_castka'], ' Kč</span>';
					else
						echo '<span style="color:#0B0;">',
							$user['up_castka'], ' Kč</span>';
				} else {
						echo '<span><a href="/admin/platby/add?u=',
							$user['u_id'], '">NE</a></span>';
				}
				echo '</td>';
				
				echo '<td>', ((((bool) $user['up_id']) &&
					strcmp($user['up_plati_do'], date('Y-m-d')) >= 0) ?
					formatDate($user['up_plati_do']) : '---'), '</td>';
				echo '</tr>';
			}
			echo '</table>';
			echo '</div>';
			echo '<br/>';
		}
		echo '</td>';
	}
	echo '</tr>';
	echo '</table>';
}

?>