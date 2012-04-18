<?php header_minor('Upload'); ?>
<form enctype="multipart/form-data" action="<?php $_SERVER['REQUEST_URI'];?>" method="post">
<table>
<tr><td>Popis:</td><td>Soubor</td></tr>
<?php
for($i = 0; $i < 5; $i++) {
	echo '<tr><td><input name="name[]" type="text" size="10"/></td>';
	echo '<td><input name="file[]" multiple="multiple" type="file"/></td></tr>';
}
echo '<tr><td>Složka: </td><td><select name="dir">';
$dirs = DBGalerie::getDirs();
foreach($dirs as $dir) {
	echo '<option value="', $dir['gd_id'], '">', str_repeat('&nbsp;&nbsp;', $dir['gd_level']),
		$dir['gd_name'], '</option>';
}
echo '</select></td></tr>';
?>
<tr>
	<td>
		<button type="submit" name="action" value="save">Uložit fotky</button>
		<a href="/admin/galerie">Zpět</a>
	</td>
</tr>
</table>
</form>