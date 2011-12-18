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
	echo '<a href="/member/clenove">Zpět</a>';
	return;
} else {
?>
<table>
<?php
	$users = DBUser::getActiveUsers(true);
	foreach($users as $user) {
		echo '<tr>';
		echo '<td><a href="/member/clenove/', $user['u_id'], '">';
		echoFullJmeno($user);
		echo '</a></td>';
		echo '</tr>';
	}
?>
</table>
<?php
}
?>