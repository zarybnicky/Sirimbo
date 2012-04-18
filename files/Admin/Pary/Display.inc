<form action="<?php echo $_SERVER["REQUEST_URI"]?>" method="POST">
<button type="submit" name="action" value="edit">Upravit</button>
<button type="submit" name="action" value="remove">Odstranit</button>
<table>
<tr>
<td></td>
<td>Partner</td>
<td>Partnerka</td>
<td>Standart</td>
<td>Latina</td>
<td>Body</td>
</tr>
<?php
$pary = DBPary::getActivePary();

foreach($pary as $par) {
	echo '<tr>';
	echo '<td><input type="checkbox" name="pary[]" value="', $par["p_id"], '" /></td>';
	echo '<td>', $par['guy_name'], " ", $par['guy_surname'], '</td>';
	echo '<td>', $par['gal_name'], " ", $par['gal_surname'], '</td>';
	echo '<td>', $par['p_stt_trida'], ' ', $par['p_stt_body'], 'F', $par['p_stt_finale'], '</td>';
	echo '<td>', $par['p_lat_trida'], ' ', $par['p_lat_body'], 'F', $par['p_lat_finale'], '</td>';
	echo '<td>', $par['p_hodnoceni'], '</td>';
	echo '</tr>';
}
?>
</table>
<br />
<?php header_minor("Nový pár:");//TODO: Přidat "Nový pár" do tabulky?>
<?php $userselect = new UserSelectHelper(); ?>
Partner: <?php echo $userselect->userSelect()->name('add_partner')
	->users(DBUser::getUsersByPohlavi("m"));?><br />
Partnerka: <?php echo $userselect->userSelect()->name('add_partnerka')
	->users(DBUser::getUsersByPohlavi("f"));?><br />
<button type="submit" name="action" value="add">Nový pár</button>
</form>