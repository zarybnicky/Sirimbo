<form action="<?php echo $_SERVER["REQUEST_URI"]?>" method="POST">
<button type="submit" name="action" value="add">Přidat uživatele</button>
</form>

<form action="<?php echo $_SERVER["REQUEST_URI"]?>" method="POST">
<button type="submit" name="action" value="edit">Upravit</button>
<button type="submit" name="action" value="remove">Odstranit</button>
<table>
<tr>
<td></td>
<td>Login</td>
<td>Jméno</td>
<td>Příjmení</td>
<td>Oprávnění</td>
</tr>
<?php
//TODO: Paging, getUsers(OD,DO)
$users = Database::getUsers();
foreach($users as $user) {
	echo "<tr>";
	if(($user["u_level"] < User::getUserLevel() && !$user["u_lock"] &&
			User::getUserName() != $user["u_login"]) || User::checkPermissionsBool(L_SADMIN))
		echo "<td><input type=\"checkbox\" name=\"users[]\" value=\"" . $user["u_login"] .
			"\" /></td>";
	else
		echo "<td><input type=\"checkbox\" name=\"users[]\" value=\"" .
			"\" disabled=\"d\" /></td>";
	echo "<td>" . $user["u_login"] . "</td>";
	echo "<td>" . $user["u_jmeno"] . "</td>";
	echo "<td>" . $user["u_prijmeni"] . "</td>";
	$ul = $user["u_level"];
	echo "<td>";
	switch($ul) {
		case L_USER:
			echo "Uživatel";
			break;
		case L_EDITOR:
			echo "Editor";
			break;
		case L_TRENER:
			echo "Trenér";
			break;
		case L_ADMIN:
			echo "Admin";
			break;
		case L_SADMIN:
			echo "Superadmin";
			break;
		default:
			echo "Neznámý";
			break;
	}
	echo "</td>";
	echo "</tr>";
}
?>
</table>
</form>