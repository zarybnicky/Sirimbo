<?php
User::checkPermissionsError(L_HOST);
header_main("Dokumenty");

$kategorie = get('kat');
?>
<form action="<?php echo $_SERVER["REQUEST_URI"];?>" method="GET">
<select name="kat">
	<option value="">---VŠE---</option>
	<option value="1"<?php if($kategorie == '1') echo ' selected'?>>Schůze, rady</option>
	<option value="2"<?php if($kategorie == '2') echo ' selected'?>>Soutěže</option>
	<option value="3"<?php if($kategorie == '3') echo ' selected'?>>Tábory</option>
	<option value="4"<?php if($kategorie == '4') echo ' selected'?>>Inzerce</option>
	<option value="0"<?php if($kategorie == '0') echo ' selected'?>>Ostatní</option>
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
if(ctype_digit($kategorie) || $kategorie == '0') {
	$dokumenty = DBDokumenty::getDokumentyByKategorie($kategorie);
} else {
	$dokumenty = DBDokumenty::getDokumenty();
}

foreach($dokumenty as $item) {
	echo '<tr>';
	echo '<td>' . $item['d_name'] .  '</td>';
	echo '<td><a href="/member/download?id=' . $item['d_id'] . '">' . $item['d_filename'] .  '</a></td>';
	echo '<td>', Settings::$document_types[$item['d_kategorie']], '</td>';
	echo '<td>';
	echoFullJmeno($item);
	echo '</td>';
	echo '</tr>';
}
?>
</table>