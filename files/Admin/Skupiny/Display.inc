<?php
if(get('v') === null)
	get('v', 'info');

if(!TISK) {
	echo '<form action="', $_SERVER['REQUEST_URI'], '" method="get">';

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
	
	echo '<table>';
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
		echo '<td><div class="box" title="', $item['us_popis'], '" ',
			'style="background-color:', Settings::$barvy[$item['us_color']][1] . ';"></div></td>';
		echo '<td style="max-width:300px;">', $item['us_popis'], '</td>';
		echo '<td class="center">', $item['us_platba_mesic'], ' Kč</td>';
		echo '<td class="center">', $item['us_platba_ctvrtrok'], ' Kč</td>';
		echo '<td class="center">', $item['us_platba_pulrok'], ' Kč</td>';
		echo '</tr>';
	}
	echo '</table>';
	echo '</form>';
} elseif(get('v') == 'users') {
	foreach($skupiny as $skupina) {
		header_minor('<div class="box" title="' . $skupina['us_popis'] .
			'" style="background-color:'. Settings::$barvy[$skupina['us_color']][1] .
			';"></div>&nbsp;&nbsp;' . $skupina['us_popis']);
		
		$users = DBUser::getUsersBySkupina($skupina['us_id']);
		if(empty($users)) {
			echo '<br/>';
			continue;
		}
		
		echo '<table>';
		echo '<tr>';
		echo '<td>Jméno</td>';
		echo '<td>Var. symbol</td>';
		echo '<td>Částka</td>';
		echo '<td>Zaplaceno do</td>';
		echo '</tr>';
		foreach($users as $user) {
			$placeno = (bool) $user['up_id'];
			$zaplaceno = $placeno && strcmp($user['up_plati_do'], date('Y-m-d')) >= 0;
			
			if($placeno && !$zaplaceno) {
				$now = time();
				$kdy = strtotime($user['up_plati_do']);
				
				$diff = $now - $kdy;
				if($diff > 31536000)
					$diffstr = 'více než rok';
				elseif($diff > 15768000)
					$diffstr = 'více než půl roku';
				elseif($diff > 7884000)
					$diffstr = 'více než 3 měsíce';
				elseif($diff > 5256000)
					$diffstr = 'více než 2 měsíce';
				elseif($diff > 2628000)
					$diffstr = 'více než měsíc';
				else
					$diffstr = 'méně než měsíc';
			}
			
			echo '<tr>';
			echo '<td style="width:140px;">', $user['u_prijmeni'], ', ', $user['u_jmeno'], '</td>';
			echo '<td>', User::var_symbol($user['u_id']), '</td>';

			echo '<td style="width:50px;">';
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
						$user['u_id'], '">NE</s></span>';
			}
			echo '</td>';
			
			echo '<td style="width:110px;">',
				($zaplaceno ? formatDate($user['up_plati_do']) :
				($placeno ? ('neplaceno ' . $diffstr) : 'nikdy neplaceno')), '</td>';
			echo '</tr>';
		}
		echo '</table>';
		echo '<br/>';
	}
}
?>