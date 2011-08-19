<form action="<?php echo $_SERVER["REQUEST_URI"] //TODO: Paging, getUpozorneni(OD,DO)?>" method="POST">
<button type="submit" name="action" value="add">Přidat</button>
</form>

<form action="<?php echo $_SERVER["REQUEST_URI"] ?>" method="POST">
<button type="submit" name="action" value="edit">Upravit</button>
<button type="submit" name="action" value="remove">Odstranit</button>
<table>
<tr>
<td></td>
<td>Přidal</td>
<td>Datum</td>
<td>Nadpis</td>
</tr>
<?php
$upozorneni= Database::getNastenka();

foreach($upozorneni as $item) {
	echo "<tr>";
	
	if(User::checkPermissionsBool(L_ADMIN) || ($item["u_login"] == User::getUserName() &&
			!Database::isNastenkaLocked($item["up_id"])))
		echo "<td><input type=\"checkbox\" name=\"nastenka[]\" value=\"" . $item["up_id"] .
			"\" /></td>";
	else
		echo "<td><input type=\"checkbox\" name=\"nastenka[]\" value=\"" .
			"\" disabled=\"d\" /></td>";
	echo "<td>" . $item["u_login"] . "</td>";
	echo "<td>" . $item["up_kdy"] . "</td>";
	echo "<td>" . $item["up_nadpis"] . "</td>";
	echo "</tr>";
}
?>
</table>
</form>