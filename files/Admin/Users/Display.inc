<?php
header_main('Správa uživatelů');
notice(View::getRedirectMessage());

if(get('v') === null)
	get('v', 'info');

$groups = DBPermissions::getGroups();

if(!TISK) {
	echo '<form action="', $_SERVER['REQUEST_URI'], '" method="get">';
	$s = Helper::get()->select();
	echo 'Uživatelé:&nbsp;';
	$s->select()
		->get()->name('f')
		->value('all')
		->option('all', 'všichni')
		->option('dancer', 'tanečníci')
		->option('system', 'systémoví')
		->option('unconfirmed', 'nepotvrzení')
		->option('ban', 'zabanovaní');
	foreach($groups as $group)
		if($group['pe_id'])
			$s->option($group['pe_id'], $group['pe_name']);
	echo $s;
	echo '&nbsp;řadit podle:&nbsp;';
	echo $s->select()
		->get()->name('s')
		->option('surname', 'příjmení')
		->option('var-symbol', 'var. symbolu')
		->option('narozeni', 'data narození');
	echo '&nbsp;zobrazení:&nbsp;';
	echo $s->select()
		->get()->name('v')
		->option('info', 'informace')
		->option('status', 'status uživatele')
		->option('platby', 'platby')
		->option('statistiky', 'statistiky');
	echo '<button type="submit">Odeslat</button>';
	echo '</form><br/>';
}

if(get('v') == 'statistiky') {
	$groupcount = DBUser::getGroupCounts();
	$all = DBUser::getUsers(L_ALL);
	$active = DBUser::getActiveUsers(L_ALL);
	$dancers = DBUser::getActiveDancers(L_ALL);
	
	echo '<table>';
	echo '<tr><td></td><td>Počet</td></tr>';
	
	$in = array(
		array('Uživatelé v databázi', count($all)),
		array('Aktivní uživatelé', count($active)),
		array('Aktivní tanečníci', count($dancers))
	);
	foreach($groupcount as $group)
		$in[] = array($group['pe_name'], $group['count']);
	
	foreach($in as $item) {
		echo '<tr>';
		echo '<td style="text-align:right;">', $item[0], '</td>';
		echo '<td style="text-align:center;"><span class="big">', $item[1], '</span></td>';
		echo '</tr>';
	}
	echo '</table>';
	return;
}

foreach($groups as $group)
	if($group['pe_id'])
		$filter[] = $group['pe_id'];

$options['sort'] = in_array(get('s'), array('prijmeni', 'narozeni', 'var-symbol')) ?
	get('s') : 'prijmeni';
$options['filter'] = in_array(get('f'), array_merge(array('dancer', 'system', 'all', 'unconfirmed', 'ban')), $filter) ?
	get('f') : 'all';
$pager = new Paging(new PagingAdapterDBSelect('DBUser', $options));
$pager->setCurrentPageField('p');
$pager->setItemsPerPageField('c');
$pager->setDefaultItemsPerPage(20);
$pager->setPageRange(5);
$items = $pager->getItems();

echo '<form action="', Request::getURI(), '" method="POST">';
if(!TISK) {
	$f = Helper::get()->menu();
	$f->float(MenuHelper::FLOAT_RIGHT)
		->content('Přidat', '/admin/users/add')
		->content('Upravit', 'edit', true)
		->content('Odebrat', 'remove', true)
		->content('Zobrazit platby', 'platby', true)
		->content('Duplicitní uživatelé', '/admin/users/duplicate');
	if(get('v') == 'status')
		$f->content('Uložit změny', 'save', true);
	echo $f;
}
if(empty($items)) {
	notice('Žádní takoví uživatelé');
	return;
}
echo '<div style="margin-right:150px">';
echo '<table style="width:100%">';
echo '<tr>';
echo '<td></td>';
echo '<td></td>';
echo '<td>Jméno</td>';
echo '<td></td>';

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
	echo '<td>Zaplaceno</td>';
	echo '<td>Zaplaceno do</td>';
}
echo '</tr>';

if(get('v') == 'status') {
	$skupiny = DBSkupiny::getSkupiny();
	$skupinyselect = Helper::get()->select()->post();
	foreach($skupiny as $skupina)
		$skupinyselect->option($skupina['us_id'], $skupina['us_popis']);
}

$groups = DBPermissions::getGroups();
foreach($groups as $group) {
	$groups_new[$group['pe_id']] = $group['pe_name'];
}
$groups = $groups_new;unset($groups_new);

$i = $pager->getItemsPerPage() * ($pager->getCurrentPage() - 1);
foreach($items as $user) {
	echo '<tr>';
	echo '<td><input type="checkbox" name="users[]" value="', $user['u_id'], '" /></td>';
	echo '<td>', ++$i, '. </td>';
	echo '<td style="max-width:140px;">', $user['u_prijmeni'], ', ', $user['u_jmeno'], '</td>';
	
	if(get('v') == 'info') {
		echo '<td>', getColorBox($user['us_color'], $user['us_popis']), '</td>';
		echo '<td>', $groups[$user['u_group']], '</td>';
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
		echo '<td>', $groups[$user['u_group']], '</td>';
		echo '<td class="center"><label>', 
			getCheckbox($user['u_id'] . '-dancer', '1', $user['u_dancer']), '</label></td>';
		echo '<td class="center"><label>',
			getCheckbox($user['u_id'] . '-system', '1', $user['u_system']), '</label></td>';
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
		echo '<td style="width:58px;">';
		if($user['up_id'] && $zaplaceno)
			echo '<span style="color:#0B0;">', $user['up_castka'], ' Kč</span>';
		else
			echo '<span><a href="/admin/platby/add?u=', $user['u_id'], '">NE</a></span>';
		echo '</td>';
		echo '<td style="width:110px;">',
			($zaplaceno ?
			formatDate($user['up_plati_do']) :
			($placeno ? $diffstr : 'nikdy')),
			'</td>';
	}
	echo '</tr>';
}
echo '</table>';
echo '</div>';
echo '</form>';
echo '<div style="text-align:center;margin-right:150px;">';
echo $pager->getNavigation();
echo '</div>';
?>