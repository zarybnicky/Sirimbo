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
		->option('platby', 'platby');
	echo '<button type="submit">Odeslat</button>';
	echo '</form><br/>';
}

echo '<form action="', $_SERVER['REQUEST_URI'], '" method="POST">';

if(!TISK) {
	$f = new MenuHelper();
	$f->menu()
		->float(MenuHelper::FLOAT_RIGHT)
		->content('Přidat', 'users/add')
		->content('Upravit', 'edit', true)
		->content('Odebrat', 'remove', true)
		->content('Nepotvrzení uživatelé', 'users/new');
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

if(get('v') == 'status') {
	$skupiny = DBSkupiny::getSkupiny();
	$skupinyselect = new SelectHelper();
	$skupinyselect->select()
		->get(false);
	foreach($skupiny as $skupina)
		$skupinyselect->option($skupina['us_id'], $skupina['us_popis']);
}

echo '<table>';
echo '<tr>';
echo '<td></td>';
echo '<td>Jméno</td>';
echo '<td>Oprávnění</td>';
echo '<td>Skupina</td>';


if(get('v') == 'info') {
	echo '<td>Datum<br/>narození</td>';
	echo '<td>Variabilní<br/>symbol</td>';
	echo '<td>Zaplaceno</td>';
} elseif(get('v') == 'status') {
	echo '<td>Aktivní<br/>tanečník</td>';
	echo '<td>Systémový<br/>uživatel</td>';
} elseif(get('v') == 'platby') {
	echo '<td>Částka</td>';
	echo '<td>Zaplaceno<br/>do</td>';
}
echo '</tr>';

foreach($users as $user) {
	echo '<tr>';
	if(Permissions::canEditUser($user['u_id'], $user['u_level'], $user['u_lock']))
		echo '<td><input type="checkbox" name="users[]" value="', $user['u_id'], '" /></td>';
	else
		echo '<td><input type="checkbox" name="users[]" value="" disabled="disabled" /></td>';
	echo '<td style="max-width:140px;">', $user['u_prijmeni'], ', ', $user['u_jmeno'], '</td>';
	
	$ul = $user['u_level'];
	echo '<td>';
	switch($ul) {
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
	
	if(get('v') == 'info') {
		echo '<td>';
		echo '<div class="box" title="', $user['us_popis'], '" ',
			'style="background-color:', Settings::$barvy[$user['us_color']][1], ';"></div>';
		echo '</td>';
		echo '<td>', formatDate($user['u_narozeni']), '</td>';
		echo '<td>', User::var_symbol($user['u_id']), '</td>';
		echo '<td>', ((bool) $user['up_id'] ? 'ano' :
			('<a href="/admin/platby/add?u=' . $user['u_id'] . '">NE</a>')), '</td>';
	} elseif(get('v') == 'status') {
		echo '<td>';
		echo $skupinyselect
			->name($user['u_id'] . '-skupina')
			->value($user['u_skupina']);
		echo '</td>';
		echo '<td class="center">', '<input type="checkbox" name="', $user['u_id'], '-dancer" ',
			($user['u_dancer'] ? 'checked="checked"' : ''), '/>', '</td>';
		echo '<td class="center">', '<input type="checkbox" name="', $user['u_id'], '-system" ',
			($user['u_system'] ? 'checked="checked"' : ''), '/>', '</td>';
	} elseif(get('v') == 'platby') {
		$now = new DateTime('now');
		$kdy = new DateTime($user['up_plati_do']);
		
		$placeno = (bool) $user['up_id'];
		$zaplaceno = $placeno && ($kdy >= $now);
		
		if($placeno && !$zaplaceno) {
			$diff = $now->diff($kdy);
			if($diff->y == 1)
				$diffstr = 'více než rok';
			elseif($diff->y > 1 && $diff->y < 5)
				$diffstr = 'více než ' . $diff->y . ' roky';
			elseif($diff->y >= 1)
				$diffstr = 'více než ' . $diff->y . 'let';
			elseif($diff->m == 1)
				$diffstr = 'více než měsíc';
			elseif($diff->m > 1 && $diff->m < 5)
				$diffstr = 'více než ' . $diff->m . ' měsíce';
			elseif($diff->m >= 5)
				$diffstr = 'více než ' . $diff->m . ' měsíců';
			else
				$diffstr = 'méně než měsíc';
		}
			
		echo '<td>';
		echo '<div class="box" title="', $user['us_popis'], '" ',
			'style="background-color:', Settings::$barvy[$user['us_color']][1], ';"></div>';
		echo '</td>';
		echo '<td style="width:105px;">',
			($zaplaceno ?
			('<a href="/admin/platby/edit/' . $user['up_id'] . '">' . $user['up_castka'] . ' Kč</a>') :
			('<a href="/admin/platby/add?u=' . $user['u_id'] . '">NEZAPLACENO</a>')),
			'</td>';
		echo '<td>',
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