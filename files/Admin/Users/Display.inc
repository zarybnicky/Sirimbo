<form action="/admin/users" method="POST">
<?php
$f = new MenuHelper();
echo $f->menu()
	->float(MenuHelper::FLOAT_RIGHT)
	->content('Přidat', 'users/add')
	->content('Upravit', 'edit', true)
	->content('Odebrat', 'remove', true)
	->content('Nepotvrzení uživatelé', 'users/new')
	->content('Uložit změny', 'save', true);
?>
<table>
<tr>
<td></td>
<td>Login</td>
<td>Jméno</td>
<td>Datum<br/>narozeni</td>
<td>Aktivní<br/>tanečník</td>
<td>Systémový<br/>uživatel</td>
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
	echo '<td style="max-width:140px;">';
	echoFullJmeno($user);
	echo '</td>';
	echo '<td>', formatDate($user['u_narozeni']), '</td>';
	echo '<td class="center">', '<input type="checkbox" name="', $user['u_id'], '-dancer" ',
		($user['u_dancer'] ? 'checked="checked"' : ''), '/>', '</td>';
	echo '<td class="center">', '<input type="checkbox" name="', $user['u_id'], '-system" ',
		($user['u_system'] ? 'checked="checked"' : ''), '/>', '</td>';
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