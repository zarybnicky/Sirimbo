<form action="<?php echo $_SERVER["REQUEST_URI"] //TODO: Paging, getUpozorneni(OD,DO)?>" method="POST">
<button type="submit" name="action" value="add">Přidat</button>
</form>

<form action="<?php echo $_SERVER["REQUEST_URI"] ?>" method="POST">
<button type="submit" name="action" value="edit">Upravit</button>
<button type="submit" name="action" value="remove">Odstranit</button>
<table>
<tr>
<td></td>
<td>Trenér</td>
<td>Datum</td>
</tr>
<?php
$nabidka = Database::getNabidka();

foreach($nabidka as $item) {
	echo "<tr>";
	
	if(User::checkPermissionsBool(L_ADMIN) || ($item["n_trener"] == User::getUserName() &&
			!Database::isNastenkaLocked($item["n_id"])))
		echo "<td><input type=\"checkbox\" name=\"nabidka[]\" value=\"" . $item["n_id"] .
			"\" /></td>";
	else
		echo "<td><input type=\"checkbox\" name=\"nabidka[]\" value=\"" .
			"\" disabled=\"d\" /></td>";
	echo "<td>" . $item["u_jmeno"] . " " . $item["u_prijmeni"] . "</td>";
	echo "<td>" . $item["n_od"] . " - " . $item["n_do"] .  "</td>";
	echo "</tr>";
}
?>
</table>
</form>