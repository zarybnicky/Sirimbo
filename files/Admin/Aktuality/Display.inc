<form action="<?php echo $_SERVER["REQUEST_URI"] ?>" method="POST">
<a href="/admin/aktuality/add">Přidat</a>
<button type="submit" name="action" value="edit">Upravit</button>
<button type="submit" name="action" value="remove">Odstranit</button>
<table>
<tr>
<td></td>
<td>Jméno</td>
<td>Kategorie</td>
</tr>
<?php
$ankety = DBAktuality::getAktuality();

foreach($ankety as $item) {
	echo '<tr>';
	
	if(Permissions::canEditAktualita($item['at_kdo']))
		echo '<td><input type="checkbox" name="aktuality[]" value="' . $item['at_id'] .
			'" /></td>';
	else
		echo '<td><input type="checkbox" name="aktuality[]" value="' .
			'" disabled="disabled" /></td>';
	echo '<td>', $item['at_jmeno'], '</td>';
	echo '<td>';
	if($item['at_kat'] == AKTUALITY_ZPRAVY) echo "Zprávy";
	elseif($item['at_kat'] == AKTUALITY_VIDEA) echo "Videa";
	echo '</td>';
	echo '</tr>';
}
?>
</table>
</form>