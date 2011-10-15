<form action="<?php echo $_SERVER["REQUEST_URI"]?>" method="POST">
<button type="submit" name="action" value="confirm_confirm">Potvrdit</button>
<button type="submit" name="action" value="remove">Odstranit</button>
<table>
<tr>
<td></td>
<td>Úroveň</td>
<td>Login</td>
<td>Jméno</td>
<td>Email</td>
</tr>
<?php
$users = DBUser::getNewUsers();
foreach($users as $user) {
	echo "<tr>";
	echo "<td><input type=\"checkbox\" name=\"users[]\" value=\"" . $user["u_login"] .
			"\" /></td>";
	echo '<td><select name="' . $user['u_login'] . '-level">
				<option value="host" selected="selected">Host</option>
				<option value="user">Uživatel</option>
				<option value="editor">Editor</option>
				<option value="trener">Trenér</option>
				<option value="admin">Administrátor</option>
		</select></td>';
	echo "<td>" . $user["u_login"] . "</td>";
	echo "<td>";
	echoFullJmeno($user);
	echo "</td>";
	echo "<td>" . $user["u_email"] . "</td>";
	echo "</tr>";
}
?>
</table>
</form>

<form action="/admin/users" method="GET">
<button type="submit">Zpět</button>
</form>