<form action="<?php echo $_SERVER["REQUEST_URI"] ?>" method="POST">
<button type="submit" name="action" value="add">Přidat</button>
<button type="submit" name="action" value="edit">Upravit</button>
<button type="submit" name="action" value="edit_detail">Upravit přihášené</button>
<button type="submit" name="action" value="edit_doku">Upravit dokumenty</button>
<button type="submit" name="action" value="remove">Odstranit</button>
<table>
<tr>
<td></td>
<td>Jméno</td>
<td>Místo</td>
<td>Datum</td>
</tr>
<?php
$tas = DBTaS::getTaS();

foreach($tas as $item) {
	echo "<tr>";
	
	echo "<td><input type=\"checkbox\" name=\"tas[]\" value=\"", $item["t_id"], "\" /></td>";
	echo "<td>", $item["t_jmeno"], "</td>";
	echo "<td>", $item["t_kde"], "</td>";
	echo "<td>", formatDate($item["t_od"]);
	if($item["t_od"] != $item["t_do"])
		echo " - ", formatDate($item["t_do"]);
	echo "</td>";
	echo "</tr>";
}
?>
</table>
</form>