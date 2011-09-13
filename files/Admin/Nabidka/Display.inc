<form action="<?php echo $_SERVER["REQUEST_URI"] //TODO: Paging, getUpozorneni(OD,DO)?>" method="POST">
<button type="submit" name="action" value="add">Přidat</button>
</form>

<form action="<?php echo $_SERVER["REQUEST_URI"] ?>" method="POST">
<button type="submit" name="action" value="edit">Upravit</button>
<button type="submit" name="action" value="edit_detail">Upravit tréninky</button>
<button type="submit" name="action" value="remove">Odstranit</button>
<table>
<tr>
<td></td>
<td>Trenér</td>
<td>Datum</td>
<td>Viditelný</td>
</tr>
<?php
$nabidka = DBNabidka::getNabidka();

foreach($nabidka as $item) {
	echo "<tr>";
	
	if(User::checkPermissionsBool(L_ADMIN) || ($item["n_trener"] == User::getUserID() &&
			!$item["n_lock"]))
		echo "<td><input type=\"checkbox\" name=\"nabidka[]\" value=\"" . $item["n_id"] .
			"\" /></td>";
	else
		echo "<td><input type=\"checkbox\" name=\"nabidka[]\" value=\"" .
			"\" disabled=\"d\" /></td>";
	echo "<td>" . $item["u_jmeno"] . " " . $item["u_prijmeni"] . "</td>";
	echo "<td>" . formatDate($item["n_od"]) . " - " . formatDate($item["n_do"]) .  "</td>";
	echo "<td>" . ($item["n_visible"] ? "x" : "_") . "</td>";
	echo "</tr>";
}
?>
</table>
</form>