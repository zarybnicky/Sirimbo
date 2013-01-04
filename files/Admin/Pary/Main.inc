<?php
User::checkPermissionsError(L_ADMIN);
header_main("Správa párů");

notice(View::getRedirectMessage());

switch(Request::getAction()) {
	case 'edit':
		$id = Request::getID();
		if(!$id || !($data = DBPary::getSinglePar($id)))
			View::redirect('/admin/pary', 'Pár s takovým ID neexistuje');

		if(empty($_POST)) {
			post('stt-trida', $data['p_stt_trida']);
			post('stt-body', $data['p_stt_body']);
			post('stt-finale', $data['p_stt_finale']);
			post('lat-trida', $data['p_lat_trida']);
			post('lat-body', $data['p_lat_body']);
			post('lat-finale', $data['p_lat_finale']);
			
			include('files/Admin/Pary/Form.inc');
			return;
		}
		$stt_body = (post('stt-body') && is_numeric(post('stt-body'))) ?
			post('stt-body') : 0;
		$stt_finale = (post('stt-finale') && is_numeric(post('stt-finale'))) ?
			post('stt-finale') : 0;
		$lat_body = (post('lat-body') && is_numeric(post('lat-body'))) ?
			post('lat-body') : 0;
		$lat_finale = (post('lat-finale') && is_numeric(post('lat-finale'))) ?
			post('lat-finale') : 0;
			
		$stt_amend = constant("AMEND_" . post('stt-trida'));
		$lat_amend = constant("AMEND_" . post('lat-trida'));
		$stt_base = ($stt_body + 40 * $stt_finale) * $stt_amend;
		$lat_base = ($lat_body + 40 * $lat_finale) * $lat_amend;
		
		$stt_bonus = constant("BONUS_" . post('stt-trida'));
		$lat_bonus = constant("BONUS_" . post('lat-trida'));
		
		$hodnoceni = $stt_base + $lat_base + $stt_bonus + $lat_bonus;
		
		DBPary::editTridaBody($data['p_id'], post('stt-trida'), $stt_body,
			$stt_finale, post('lat-trida'), $lat_body, $lat_finale, $hodnoceni);
		
		View::redirect('/admin/pary', 'Třída a body změněny');
		break;
}

if(empty($_POST)) {
	include("files/Admin/Pary/Display.inc");
	return;
}
switch(post("action")) {
	case "remove":
		if(is_array(post("pary"))) {
			list($par) = post("pary");
			$data = DBPary::getSinglePar($par);
			
			if($data['guy_id'])
				DBPary::noPartner($data['guy_id']);
			if($data['gal_id'])
				DBPary::noPartner($data['gal_id']);
			
			notice('Pár odstraněn');
		}
		break;
		
	case 'add':
		$old_gal = DBPary::getLatestPartner(post("add_partner"), 'm');
		$old_guy = DBPary::getLatestPartner(post("add_partnerka"), 'f');
		
		if(post("add_partner"))
			DBPary::newPartner(post("add_partner"), post("add_partnerka"));
		if($old_guy['u_id'])
			DBPary::noPartner($old_guy['u_id']);
		if($old_gal['u_id'])
			DBPary::noPartner($old_gal['u_id']);
		
		notice('Pár přidán');
		break;
	
	case 'edit':
		$pary = post('pary');
		if($pary[0]) {
			header('Location: /admin/pary/edit/' . $pary[0]);
			return;
		}
		break;
}

include("files/Admin/Pary/Display.inc");
return;
?>