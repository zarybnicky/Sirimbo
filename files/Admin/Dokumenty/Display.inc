<form action="<?php echo $_SERVER["REQUEST_URI"] //TODO: Paging, getUpozorneni(OD,DO) ?>" method="POST">
<button type="submit" name="action" value="edit">Přejmenovat</button>
<button type="submit" name="action" value="remove">Odstranit</button>
<table>
<tr>
<td></td>
<td>Jméno souboru</td>
<td>Soubor</td>
<td>Kategorie</td>
<td>Uploadoval</td>
</tr>
<?php
$dokumenty = DBDokumenty::getDokumenty();

foreach($dokumenty as $item) {
	echo "<tr>";
	if(User::checkPermissionsBool(L_ADMIN) || $item["d_kdo"] == User::getUserID())
		echo "<td><input type=\"checkbox\" name=\"dokumenty[]\" value=\"", $item["d_id"],
			"\" /></td>";
	else
		echo "<td><input type=\"checkbox\" name=\"dokumenty[]\" value=\"",
			"\" disabled=\"disabled\" /></td>";
	echo "<td>", $item["d_name"], "</td>";
	echo "<td>", $item["d_filename"], "</td>";
	echo "<td>", Settings::$document_types[$item["d_kategorie"]], "</td>";
	echo "<td>";
	echoFullJmeno($item);
	echo "</td>";
	echo "</tr>";
}
?>
</table>
</form>
<hr />
<div class="h_minor">Upload</div>
<form action="<?php echo $_SERVER["REQUEST_URI"]?>" method="post" enctype="multipart/form-data">
<table>
<tr><td>Jméno:</td><td><input type="text" name="name" /></td></tr>
<tr><td>Soubor:</td><td><input type="file" name="file" /></td></tr>
<tr><td>Kategorie</td><td><select name="kategorie">
	<option value="1">Schůze, rady</option>
	<option value="2">Soutěže</option>
	<option value="3">Tábory</option>
	<option value="0">Ostatní</option>
</select></td></tr>
<tr><td></td><td><button type="submit" name="action" value="upload">Upload</button></td></tr>
</table>
</form>