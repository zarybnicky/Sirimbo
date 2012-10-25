<?php
User::checkPermissionsError(L_HOST);
header_main("Členové");

$request = explode('/', $_SERVER['REQUEST_URI']);
$action = array_slice($request, 2);
$id = isset($action[1]) ? $action[1] : '';

if($id && is_numeric($id) && ($data = DBUser::getUserData($id))) {
	echo '<b>', $data['u_jmeno'], ' ', $data['u_prijmeni'], '</b><br/>';
	echo ($data['u_pohlavi'] == 'm' ? 'Pohlaví: muž' : 'Pohlavi: žena'), '<br />';
	echo 'E-mail: ', $data['u_email'], '<br/>';
	echo 'Telefon: ', $data['u_telefon'], '<br/>';
	echo 'Poznámky: ', $data['u_poznamky'], '<br/>';
	echo 'Variabilní symbol: ', User::var_symbol($id), '<br/>';
	echo '<a href="/member/clenove">Zpět</a>';
	return;
}

$filter = get('f');
switch($filter) {
	case 'dancer': $users = DBUser::getActiveDancers(); break;
	case 'trener': $users = DBUser::getActiveUsers(L_TRENER); break;
	case 'editor': $users = DBUser::getActiveUsers(L_EDITOR); break;
	case 'all':
	default: $users = DBUser::getActiveUsers(L_ALL); break;
}
$count = count($users);

echo '<form action="', $_SERVER['REQUEST_URI'], '" method="get">';
echo 'Uživatelé:&nbsp;<select name="f">';
echo '<option value="all"', (get('f') == 'all') ? ' selected="selected"' : '', '>všichni</option>';
echo '<option value="dancer"', (get('f') == 'dancer') ? ' selected="selected"' : '', '>tanečníci</option>';
echo '<option value="trener"', (get('f') == 'trener') ? ' selected="selected"' : '', '>trenéři</option>';
echo '<option value="editor"', (get('f') == 'editor') ? ' selected="selected"' : '', '>editoři</option>';
echo '</select>';
echo '<button type="submit">Odeslat</button>';
echo '</form>';

echo '<br/>';
echo 'Celkem:&nbsp;<span class="big">', $count, '</span><br/><br/>';
echo '<table>';
$i = 1;
foreach($users as $user) {
	echo '<tr>';
	echo '<td>&nbsp;', $i++, '.</td>';
	echo '<td><a href="/member/clenove/', $user['u_id'], '">';
	echo $user['u_prijmeni'] . ', ' . $user['u_jmeno'];
	echo '</a></td>';
	echo '<td>';
	if($user['u_dancer']) {
		if($user['u_pohlavi'] == 'm')
			echo 'tanečník';
		else
			echo 'tanečnice';
	}
	echo '</td>';
	echo '</tr>';
}
echo '</table>';
echo '<br/>';
?>