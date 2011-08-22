<div class="h_section">Dokumenty</div>
<table>
<tr>
<td>Jméno souboru</td>
<td>Soubor</td>
<td>Kategorie</td>
<td>Uploadoval</td>
</tr>
<?php
User::checkPermissionsError(L_USER);

$dokumenty = Database::getDokumenty();

foreach($dokumenty as $item) {
	echo "<tr>";
	echo "<td>" . $item["d_name"] .  "</td>";
	echo "<td><a href=\"" . "/" . $item["d_path"] . "\">" . $item["d_filename"] .  "</a></td>";
	//TODO: Kategorie
	echo "<td>", Settings::$document_types[$item["d_kategorie"]], "</td>";
	echo "<td>" . $item["u_jmeno"] . " " . $item["u_prijmeni"] . "</td>";
	echo "</tr>";
}
?>
</table>