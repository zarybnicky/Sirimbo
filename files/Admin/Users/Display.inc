<?php
if(get('v') === null)
	get('v', 'info');

if(!TISK) {
	echo '<form action="', $_SERVER['REQUEST_URI'], '" method="get">';

	$s = new SelectHelper();
	echo 'Uživatelé:&nbsp;';
	echo $s->select()
		->name('f')
		->option('all', 'všichni')
		->option('dancer', 'tanečníci')
		->option('trener', 'trenéři')
		->option('editor', 'editoři');
	echo '&nbsp;řadit podle:&nbsp;';
	echo $s->select()
		->name('s')
		->option('surname', 'příjmení')
		->option('var-symbol', 'var. symbolu')
		->option('narozeni', 'data narození');
	echo '&nbsp;zobrazení:&nbsp;';
	echo $s->select()
		->name('v')
		->option('info', 'informace')
		->option('status', 'status uživatele')
		->option('platby', 'platby')
		->option('statistiky', 'statistiky');
	echo '<button type="submit">Odeslat</button>';
	echo '</form><br/>';
}

if(get('v') == 'statistiky') {
	$all = DBUser::getUsers(L_ALL);
	$active = DBUser::getActiveUsers(L_ALL);
	$dancers = DBUser::getActiveDancers(L_ALL);
	$hosti = DBUser::getActiveUsers(L_HOST);
	$members = DBUser::getActiveUsers(L_USER);
	$editori = DBUser::getActiveUsers(L_EDITOR);
	$treneri = DBUser::getActiveUsers(L_TRENER);
	$admini = DBUser::getActiveUsers(L_ADMIN);
	
	echo '<table>';
	echo '<tr><td></td><td>Počet</td></tr>';
	
	$in = array(
		array('Uživatelé v databázi', count($all)),
		array('Aktivní uživatelé', count($active)),
		array('Aktivní tanečníci', count($dancers)),
		array('Hosti', count($hosti)),
		array('Členové', count($members)),
		array('Editoři', count($editori)),
		array('Trenéři', count($treneri)),
		array('Admini', count($admini)),
		array('Superadmini', '1')
	);
	
	foreach($in as $item) {
		echo '<tr>';
		echo '<td style="text-align:right;">', $item[0], '</td>';
		echo '<td style="text-align:center;"><span class="big">', $item[1], '</span></td>';
		echo '</tr>';
	}
	echo '</table>';
	return;
}

echo '<form action="', $_SERVER['REQUEST_URI'], '" method="POST">';

if(!TISK) {
	$f = new MenuHelper();
	$f->menu()
		->float(MenuHelper::FLOAT_RIGHT)
		->content('Přidat', 'users/add')
		->content('Upravit', 'edit', true)
		->content('Odebrat', 'remove', true)
		->content('Nepotvrzení uživatelé', 'users/new')
		->content('Zobrazit platby', 'platby', true);
	if(get('v') == 'status')
		$f->content('Uložit změny', 'save', true);
	
	echo $f;
}

//TODO: Paging, getUsers(OD,DO)

$sort = get('s');
$filter = get('f');
switch($filter) {
	case 'dancer': $users = DBUser::getActiveDancers(); break;
	case 'trener': $users = DBUser::getActiveUsers(L_TRENER); break;
	case 'editor': $users = DBUser::getActiveUsers(L_EDITOR); break;
	case 'all':
	default: $users = DBUser::getUsers(L_ALL); break;
}
switch($sort) {
	case 'var-symbol':
		usort($users, create_function('$a, $b',
			'return User::var_symbol($a["u_id"]) > User::var_symbol($b["u_id"]);'));
		break;
	case 'surname':
		usort($users, create_function('$a, $b', 'return $a["u_prijmeni"] > $b["u_prijmeni"];'));
		break;
	case 'narozeni':
		usort($users, create_function('$a, $b', 'return $a["u_narozeni"] > $b["u_narozeni"];'));
		break;
	default:
		break;
}

echo '<table>';
echo '<tr>';
echo '<td></td>';
echo '<td></td>';
echo '<td>Jméno</td>';
echo '<td>Skupina</td>';

if(get('v') == 'info') {
	echo '<td>Oprávnění</td>';
	echo '<td>Datum<br/>narození</td>';
	echo '<td>Zaplaceno</td>';
} elseif(get('v') == 'status') {
	echo '<td>Oprávnění</td>';
	echo '<td>Aktivní<br/>tanečník</td>';
	echo '<td>Systémový<br/>uživatel</td>';
} elseif(get('v') == 'platby') {
	echo '<td>Variabilní<br/>symbol</td>';
	echo '<td>Částka</td>';
	echo '<td>Zaplaceno<br/>do</td>';
}
echo '</tr>';

if(get('v') == 'status') {
	$skupiny = DBSkupiny::getSkupiny();
	$skupinyselect = new SelectHelper();
	$skupinyselect->select()
		->get(false);
	foreach($skupiny as $skupina)
		$skupinyselect->option($skupina['us_id'], $skupina['us_popis']);
}

$i = 0;
foreach($users as $user) {
	echo '<tr>';
	if(Permissions::canEditUser($user['u_id'], $user['u_level'], $user['u_lock']))
		echo '<td><input type="checkbox" name="users[]" value="', $user['u_id'], '" /></td>';
	else
		echo '<td><input type="checkbox" name="users[]" value="" disabled="disabled" /></td>';
	echo '<td>', ++$i, '. </td>';
	echo '<td style="max-width:140px;">', $user['u_prijmeni'], ', ', $user['u_jmeno'], '</td>';
	
	if(get('v') == 'info') {
		echo '<td>';
		echo '<div class="box" title="', $user['us_popis'], '" ',
			'style="background-color:', Settings::$barvy[$user['us_color']][1], ';"></div>';
		echo '</td>';
		echo '<td>';
		switch($user['u_level']) {
			case L_UNCONFIRMED:	echo 'Nepotvrzený uživatel'; break;
			case L_HOST:		echo 'Host';		break;
			case L_USER:		echo 'Uživatel';	break;
			case L_EDITOR:		echo 'Editor';		break;
			case L_TRENER:		echo 'Trenér';		break;
			case L_ADMIN:		echo 'Admin';		break;
			case L_SADMIN:		echo 'Superadmin';	break;
			default:			echo 'Neznámý';		break;
		}
		echo '</td>';
		echo '<td>', formatDate($user['u_narozeni']), '</td>';
		echo '<td>', (((bool) $user['up_id'] && strcmp($user['up_plati_do'], date('Y-m-d')) >= 0) ?
			'<span style="color:#0B0;">ano</span>' :
			('<a href="/admin/platby/add?u=' . $user['u_id'] . '">NE</a>')), '</td>';
	} elseif(get('v') == 'status') {
		echo '<td>';
		echo $skupinyselect
			->name($user['u_id'] . '-skupina')
			->value($user['u_skupina']);
		echo '</td>';
		echo '<td>';
		switch($user['u_level']) {
			case L_UNCONFIRMED:	echo 'Nepotvrzený uživatel'; break;
			case L_HOST:		echo 'Host';		break;
			case L_USER:		echo 'Uživatel';	break;
			case L_EDITOR:		echo 'Editor';		break;
			case L_TRENER:		echo 'Trenér';		break;
			case L_ADMIN:		echo 'Admin';		break;
			case L_SADMIN:		echo 'Superadmin';	break;
			default:			echo 'Neznámý';		break;
		}
		echo '</td>';
		echo '<td class="center">', '<input type="checkbox" name="', $user['u_id'], '-dancer" ',
			($user['u_dancer'] ? 'checked="checked"' : ''), '/>', '</td>';
		echo '<td class="center">', '<input type="checkbox" name="', $user['u_id'], '-system" ',
			($user['u_system'] ? 'checked="checked"' : ''), '/>', '</td>';
	} elseif(get('v') == 'platby') {
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
		
		echo '<td>';
		echo '<div class="box" title="', $user['us_popis'], '" ',
			'style="background-color:', Settings::$barvy[$user['us_color']][1], ';"></div>';
		echo '</td>';
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
			($zaplaceno ?
			formatDate($user['up_plati_do']) :
			($placeno ? ('neplaceno ' . $diffstr) : 'nikdy neplaceno')),
			'</td>';
	}
	echo '</tr>';
}
echo '</table>';
echo '</form>';
?>