<?php
$data = DBPary::getSinglePar($this->id);
$guy_partners = DBPary::getPreviousPartners($data['guy_id']);
$gal_partners = DBPary::getPreviousPartners($data['gal_id']);

if(!$data) {
	$this->redirect->sendRedirect("/member/zebricek", "Takový pár neexistuje");
}

echo $data['guy_name'], ' ', $data['guy_surname'], ' - ',
	$data['gal_name'], ' ', $data['gal_surname'], '<br />';
echo 'STT: ', $data['p_stt_trida'], ' - ',
	$data['p_stt_body'], 'F', $data['p_stt_finale'], '<br />';
echo 'LAT: ', $data['p_lat_trida'], ' - ',
	$data['p_lat_body'], 'F', $data['p_lat_finale'], '<br />';
echo 'Hodnocení: ', $data['p_hodnoceni'], '<br /><br />';

echo 'Předchozí partneři - ', $data['guy_name'], ' ', $data['guy_surname'], ':<br/>';
if(!$guy_partners)
	echo 'Žádní evidovaní';
else
	foreach($guy_partners as $par) {
		echo $par['guy_name'], ' ', $par['guy_surname'], ' - ',
			$par['gal_name'], '&nbsp;', $par['gal_surname'], ': (',
			'STT: ', $par['p_stt_trida'], ' - ',
			$par['p_stt_body'], 'F', $par['p_stt_finale'], ', ',
			'LAT: ', $par['p_lat_trida'], ' - ',
			$par['p_lat_body'], 'F', $par['p_lat_finale'], ', ',
			'Olymp: ', $par['p_hodnoceni'], ')<br />';
	}
echo '<br/>';

echo 'Předchozí partneři - ', $data['gal_name'], ' ', $data['gal_surname'], ':<br/>';
if(!$gal_partners)
	echo 'Žádní evidovaní';
else
	foreach($gal_partners as $par) {
		echo $par['guy_name'], ' ', $par['guy_surname'], ' - ',
			$par['gal_name'], '&nbsp;', $par['gal_surname'], ': (',
			'STT: ', $par['p_stt_trida'], ' - ',
			$par['p_stt_body'], 'F', $par['p_stt_finale'], ', ',
			'LAT: ', $par['p_lat_trida'], ' - ',
			$par['p_lat_body'], 'F', $par['p_lat_finale'], ', ',
			'Olymp: ', $par['p_hodnoceni'], ')<br />';
	}
echo '<br />';
echo '<a href="/member/zebricek">Zpět</a>';