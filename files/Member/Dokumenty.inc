<?php header_main("Dokumenty"); ?>
<form action="<?php echo $_SERVER["REQUEST_URI"]; ?>" method="GET">
<select name="kategorie">
	<option value="">---VŠE---</option>
	<option value="1">Schůze, rady</option>
	<option value="2">Soutěže</option>
	<option value="3">Tábory</option>
	<option value="0">Ostatní</option>
</select>
<button type="submit">Zobrazit</button>
</form>

<table>
<tr>
<td>Jméno souboru</td>
<td>Soubor</td>
<td>Kategorie</td>
<td>Uploadoval</td>
</tr>
<?php
User::checkPermissionsError(L_HOST);

if(getGetField("kategorie") && getGetField("kategorie") !== "") {
	$dokumenty = DBDokumenty::getDokumentyByKategorie(getGetField("kategorie"));
} else {
	$dokumenty = DBDokumenty::getDokumenty();
}

foreach($dokumenty as $item) {
	echo "<tr>";
	echo "<td>" . $item["d_name"] .  "</td>";
	echo "<td><a href=\"" . "/" . $item["d_path"] . "\">" . $item["d_filename"] .  "</a></td>";
	echo "<td>", Settings::$document_types[$item["d_kategorie"]], "</td>";
	echo "<td>" . $item["u_jmeno"] . " " . $item["u_prijmeni"] . "</td>";
	echo "</tr>";
}
?>
</table>