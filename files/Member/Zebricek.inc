<?php
header_main('Žebříček párů');
notice(View::getRedirectMessage());
$pary = DBPary::getActiveParyByHodnoceni();
if(empty($pary)) {
	notice("Žádné páry");
	return;
}
?>
<table>
<tr><td></td><td>Partner</td><td>Partnerka</td><td>Standart</td><td>Latina</td><td>Body</td></tr>
<?php
foreach($pary as $par) {
	echo '<tr><td>',
		'<a href="/member/pary/', $par['p_id'], '">Profil</a></td><td>',
		$par['guy_name'], ' ', $par['guy_surname'], '</td><td>',
		$par['gal_name'], ' ', $par['gal_surname'], '</td><td>',
		$par['p_stt_trida'], ' ', $par['p_stt_body'], 'F', $par['p_stt_finale'], '</td><td>',
		$par['p_lat_trida'], ' ', $par['p_lat_body'], 'F', $par['p_lat_finale'], '</td><td>',
		$par['p_hodnoceni'], '</td></tr>';
}
?>
</table>