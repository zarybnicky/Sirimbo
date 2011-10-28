<?php
User::checkPermissionsError(L_ADMIN);
header_main("Správa párů");

$request = explode('/', $_SERVER['REQUEST_URI']);
$action = array_slice($request, 3);
if(empty($action)) $action[] = '';

if($action[0] == 'edit' && is_numeric($action[1])) {
	$id = $action[1];
	
	if($id) {
		$data = DBPary::getSinglePar($id);
		
		$_POST['stt-trida'] = $data['p_stt_trida'];
		$_POST['stt-body'] = $data['p_stt_body'];
		$_POST['stt-finale'] = $data['p_stt_finale'];
		$_POST['lat-trida'] = $data['p_lat_trida'];
		$_POST['lat-body'] = $data['p_lat_body'];
		$_POST['lat-finale'] = $data['p_lat_finale'];
		
		include('files/Admin/Pary/Form.inc');
		return;
	}
}

if(empty($_POST)) {
	include("files/Admin/Pary/Display.inc");
	return;
}
//TODO: "Historie" partnerů
switch(getPostField("action")) {
	case "remove":
		if(is_array(getPostField("pary"))) {
			list($par) = getPostField("pary");
			$data = DBPary::getSinglePar($par);
			
			if($data['guy_id'])
				DBPary::noPartner($data['guy_id']);
			if($data['gal_id'])
				DBPary::noPartner($data['gal_id']);
			
			notice('A jeden pár se nám rozvedl :o(');
		}
		break;
		
	case 'add':
		$old_gal = DBPary::getLatestPartner(getPostField("add_partner"), 'm');
		$old_guy = DBPary::getLatestPartner(getPostField("add_partnerka"), 'f');
		
		DBPary::newPartner(getPostField("add_partner"), getPostField("add_partnerka"));
		if($old_guy['u_id'])
			DBPary::noPartner($old_guy['u_id']);
		if($old_gal['u_id'])
			DBPary::noPartner($old_gal['u_id']);
		
		notice('Tadá! A máme tu nový pár...');
		break;
	
	case 'edit':
		$pary = getPostField('pary');
		if($pary[0]) {
			header('Location: /admin/pary/edit/' . $pary[0]);
			return;
		}
		break;
		
	case 'edit_confirm':
		$partner = DBPary::getSinglePar(getPostField('id'));
		
		if(!getPostField('stt-body') || getPostField('stt-body') > 1000 ||
				getPostField('stt-body') < 0) {
			$stt_body = 0;
		} else
			$stt_body = getPostField('stt-body');
		if(!getPostField('stt-finale') || getPostField('stt-finale') > 5 ||
				getPostField('stt-finale') < 0) {
			$stt_finale = 0;
		} else
			$stt_finale = getPostField('stt-finale');
		if(!getPostField('lat-body') || getPostField('lat-body') > 1000 ||
				getPostField('lat-body') < 0) {
			$lat_body = 0;
		} else
			$lat_body = getPostField('lat-body');
		if(!getPostField('lat-finale') || getPostField('lat-finale') > 5 ||
				getPostField('lat-finale') < 0) {
			$lat_finale = 0;
		} else
			$lat_finale = getPostField('lat-finale');
			
		$stt_amend = constant("AMEND_" . getPostField('stt-trida'));
		$lat_amend = constant("AMEND_" . getPostField('lat-trida'));
		$stt_base = ($stt_body + 40 * $stt_finale) * $stt_amend;
		$lat_base = ($lat_body + 40 * $lat_finale) * $lat_amend;
		
		$stt_bonus = constant("BONUS_" . getPostField('stt-trida'));
		$lat_bonus = constant("BONUS_" . getPostField('lat-trida'));
		
		$hodnoceni = $stt_base + $lat_base + $stt_bonus + $lat_bonus;
		
		DBPary::editTridaBody($partner['p_id'], getPostField('stt-trida'), $stt_body,
			$stt_finale, getPostField('lat-trida'), $lat_body, $lat_finale, $hodnoceni);
			
		notice("Třída a body změněny");
		break;
}

include("files/Admin/Pary/Display.inc");
return;
?>