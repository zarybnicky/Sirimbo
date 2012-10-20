<form action="<?php echo $_SERVER["REQUEST_URI"]?>" method="POST">
<?php
$f = new MenuHelper();
echo $f->menu()
	->float(MenuHelper::FLOAT_RIGHT)
	->content('Potvrdit', 'confirm', true)
	->content('Odstranit', 'remove', true)
	->content('Zpět', '../users');
?>
<table>
<tr>
<td></td>
<td>Úroveň</td>
<td>Aktivní<br/>tanečník</td>
<td>Login</td>
<td>Jméno</td>
<td>Email</td>
<td>Datum<br/>narozeni</td>
</tr>
<?php
$users = DBUser::getNewUsers();
foreach($users as $user) {
	echo '<tr>';
	echo '<td><input type="checkbox" name="users[]" value="' . $user['u_id'] . '" /></td>';
	echo '<td><select name="' . $user['u_id'] . '-level">
				<option value="host" selected="selected">Host</option>
				<option value="user">Uživatel</option>
				<option value="editor">Editor</option>
				<option value="trener">Trenér</option>
				<option value="admin">Administrátor</option>
		</select></td>';
	echo '<td><input type="checkbox" name="' . $user['u_id'] . '-dancer" value="dancer" /></td>';
	echo '<td>' . $user['u_login'] . '</td>';
	echo '<td style="max-width:140px;">';
	echoFullJmeno($user);
	echo '</td>';
	echo '<td>', $user['u_email'], '</td>';
	echo '<td>', formatDate($user['u_narozeni']), '</td>';
	echo '</tr>';
}
?>
</table>
</form>