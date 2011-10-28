<form action="<?php echo $_SERVER["REQUEST_URI"] ?>" method="POST">
<a href="/admin/rozpis/add">Přidat</a>
<button type="submit" name="action" value="edit">Upravit</button>
<button type="submit" name="action" value="edit_detail">Upravit tréninky</button>
<button type="submit" name="action" value="remove">Odstranit</button>
<table>
<tr>
<td></td>
<td>Trenér</td>
<td>Datum</td>
<td>Místo</td>
<td>Viditelný</td>
</tr>
<?php
$nabidka = DBRozpis::getRozpis();

foreach($nabidka as $item) {
	echo "<tr>";
	
	if(User::checkPermissionsBool(L_ADMIN) || ($item["r_trener"] == User::getUserID() &&
			!$item["r_lock"]))
		echo "<td><input type=\"checkbox\" name=\"rozpis[]\" value=\"", $item["r_id"] .
			"\" /></td>";
	else
		echo "<td><input type=\"checkbox\" name=\"rozpis[]\" value=\"",
			"\" disabled=\"d\" /></td>";
	echo "<td>";
	echoFullJmeno($item);
	echo "</td>";
	echo "<td>", formatDate($item["r_datum"]),  "</td>";
	echo "<td>", $item["r_kde"], "</td>";
	echo "<td>", ($item["r_visible"] ? "x" : "_"), "</td>";
	echo "</tr>";
}
?>
</table>
</form>