<?php
header_main('Žebříček párů');
$pary = DBPary::getActiveParyByHodnoceni();
?>
<table>
<tr><td>Partner</td><td>Partnerka</td><td>Standart</td><td>Latina</td><td>Body</td></tr>
<?php
foreach($pary as $par) {
	echo '<tr><td>',
		$par['guy_name'], ' ', $par['guy_surname'], '</td><td>',
		$par['gal_name'], ' ', $par['gal_surname'], '</td><td>',
		$par['p_stt_trida'], ' ', $par['p_stt_body'], 'F', $par['p_stt_finale'], '</td><td>',
		$par['p_lat_trida'], ' ', $par['p_lat_body'], 'F', $par['p_lat_finale'], '</td><td>',
		$par['p_hodnoceni'], '</td></tr>';
}
?>
</table>