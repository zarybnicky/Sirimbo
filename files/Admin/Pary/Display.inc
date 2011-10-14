<form action="<?php echo $_SERVER["REQUEST_URI"]?>" method="POST">
<button type="submit" name="action" value="remove">Odstranit</button>
<table>
<tr>
<td></td>
<td>Partner</td>
<td>Partnerka</td>
</tr>
<?php
$pary = DBPary::getActivePary();

foreach($pary as $par) {
	echo '<tr>';
	echo '<td><input type="checkbox" name="pary[]" value="' . $par["p_id"] .'" /></td>';
	echo '<td>' . $par['guy_name'] . $par['guy_surname'] . '</td>';
	echo '<td>' . $par['gal_name'] . $par['gal_surname'] . '</td>';
	echo '</tr>';
}
?>
</table>
<br />
Nový pár:<br />
Partner: <?php echoUsers("add_partner", DBUser::getUsersByPohlavi("m"));?><br />
Partnerka: <?php echoUsers("add_partnerka", DBUser::getUsersByPohlavi("f"));?><br />
<button type="submit" name="action" value="add">Nový pár</button>
</form>