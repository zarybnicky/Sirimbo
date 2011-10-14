<?php
User::checkPermissionsError(L_ADMIN);

header_main("Správa párů");

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
}

include("files/Admin/Pary/Display.inc");
return;
?>