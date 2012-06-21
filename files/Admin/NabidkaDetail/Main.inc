<?php
User::checkPermissionsError(L_TRENER);
header_main("Správa nabídek");

notice(View::getRedirectMessage());

$request = explode('/', $_SERVER['REQUEST_URI']);
$action = array_slice($request, 3);
$id = isset($action[1]) ? $action[1] : '';

if(!$id || !is_numeric($id) || !($data = DBNabidka::getSingleNabidka($id)))
	View::redirect('/admin/nabidka', 'Nabídka s takovým ID neexistuje');
	
if(!Permissions::canEditNabidka($data['n_trener']))
	View::viewError(ER_AUTHORIZATION);

$items = DBNabidka::getNabidkaItem($id);
$obsazeno = DBNabidka::getNabidkaItemLessons($id);
$users = DBPary::getPartners();

if(empty($_POST)) {
	include("files/Admin/NabidkaDetail/Display.inc");
	return;
}

//-----Odebrání položky-----//
if(post("remove") > 0) {
	DBNabidka::removeNabidkaItem($id, post(post("remove") . "-partner"));
	$items = DBNabidka::getNabidkaItem($id);
	$obsazeno = DBNabidka::getNabidkaItemLessons($id);
}

//-----Jednotlivé položky nabídky-----//
foreach($items as $item) {
	if(post($item["ni_id"] . "-partner") != $item["ni_partner"] ||
			post($item["ni_id"] . "-hodiny") != $item["ni_pocet_hod"]) {
		$rozdil_hod = post($item["ni_id"] . "-hodiny") - $item["ni_pocet_hod"];
		if(($obsazeno + $rozdil_hod) > $data["n_pocet_hod"]) {
			post("pocet_hod", $obsazeno + $rozdil_hod);
		}
		DBNabidka::editNabidkaItem($item["ni_id"], post($item["ni_id"] . "-partner"),
			post($item["ni_id"] . "-hodiny"));
	}
}
$items = DBNabidka::getNabidkaItem($id);
$obsazeno = DBNabidka::getNabidkaItemLessons($id);

//-----Přidávání položek-----//
if(is_numeric(post("add_hodiny")) && is_numeric(post("add_partner"))) {
	$hodiny = post("add_hodiny");
	$partner = post("add_partner");
	
	if(is_numeric($hodiny) && is_numeric($partner)) {
		if(($obsazeno + $hodiny) > post("pocet_hod")) {
			post("pocet_hod", $obsazeno + $hodiny);
		}
		
		DBNabidka::addNabidkaItemLessons(post("add_partner"),$id,
			post("add_hodiny"));
		post('add_partner', null);
		$items = DBNabidka::getNabidkaItem($id);
		$obsazeno = DBNabidka::getNabidkaItemLessons($id);
	}
}
//TODO: Spojit R,N-detail a R,N-edit

//-----Dorovnávání skutečného a nastaveného počtu hodin-----//
if(post("pocet_hod") > $data["n_pocet_hod"]) {
	if(post("pocet_hod") < $obsazeno) {
		post("pocet_hod", $obsazeno);
	}
	DBNabidka::editNabidka($id, $data["n_trener"], post("pocet_hod"),
		$data["n_od"], $data["n_do"], $data['n_visible'], ($data["n_lock"]) ? 1 : 0);
	$data = DBNabidka::getSingleNabidka($id);
}

include("files/Admin/NabidkaDetail/Display.inc");
return;
?>