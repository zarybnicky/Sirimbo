<form action="/admin/users" method="POST">
<a href="/admin/users/add">Přidat</a>
<button type="submit" name="action" value="edit">Upravit</button>
<button type="submit" name="action" value="remove">Odstranit</button>
<a href="/admin/users/new">Nepotvrzení uživatelé</a>
<table>
<tr>
<td></td>
<td>Login</td>
<td>Jméno</td>
<td>Oprávnění</td>
</tr>
<?php
//TODO: Paging, getUsers(OD,DO)
$users = DBUser::getUsers();
foreach($users as $user) {
	echo '<tr>';
	if(($user['u_level'] < User::getUserLevel() && !$user['u_lock'] &&
			User::getUserID() != $user['u_id']) || User::checkPermissionsBool(L_SADMIN))
		echo '<td><input type="checkbox" name="users[]" value="', $user['u_id'], '" /></td>';
	else
		echo '<td><input type="checkbox" name="users[]" value="" disabled="disabled" /></td>';
	echo '<td>', $user['u_login'], '</td>';
	echo '<td>';
	echoFullJmeno($user);
	echo '</td>';
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
	echo '</tr>';
}
?>
</table>
</form>