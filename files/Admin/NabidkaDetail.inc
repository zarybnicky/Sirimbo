<?php
User::checkPermissionsError(L_TRENER);
header_main("Správa nabídek");

notice(View::getRedirectMessage());

$request = explode('/', $_SERVER['REQUEST_URI']);
$action = array_slice($request, 3);
$id = isset($action[1]) ? $action[1] : '';

if(!id || !is_numeric($id) || !($data = DBNabidka::getSingleNabidka($id)))
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
if(getPostField("remove") > 0) {
	DBNabidka::removeNabidkaItem($id, getPostField(getPostField("remove") . "-partner"));
	$items = DBNabidka::getNabidkaItem($id);
	$obsazeno = DBNabidka::getNabidkaItemLessons($id);
}

//-----Jednotlivé položky nabídky-----//
foreach($items as $item) {
	if(getPostField($item["ni_id"] . "-partner") != $item["ni_partner"] ||
			getPostField($item["ni_id"] . "-hodiny") != $item["ni_pocet_hod"]) {
		$rozdil_hod = getPostField($item["ni_id"] . "-hodiny") - $item["ni_pocet_hod"];
		if(($obsazeno + $rozdil_hod) > $data["n_pocet_hod"]) {
			$_POST["pocet_hod"] = $obsazeno + $rozdil_hod;
		}
		DBNabidka::editNabidkaItem($item["ni_id"], getPostField($item["ni_id"] . "-partner"),
			getPostField($item["ni_id"] . "-hodiny"));
	}
}
$items = DBNabidka::getNabidkaItem($id);
$obsazeno = DBNabidka::getNabidkaItemLessons($id);

//-----Přidávání položek-----//
if(is_numeric(getPostField("add_hodiny")) && is_numeric(getPostField("add_partner"))) {
	$hodiny = getPostField("add_hodiny");
	$partner = getPostField("add_partner");
	
	if(is_numeric($hodiny) && is_numeric($partner)) {
		if(($obsazeno + $hodiny) > getPostField("pocet_hod")) {
			$_POST["pocet_hod"] = $obsazeno + $hodiny;
		}
		
		DBNabidka::addNabidkaItemLessons(getPostField("add_partner"),$id,
			getPostField("add_hodiny"));
		unset($_POST["add_partner"]);
		$items = DBNabidka::getNabidkaItem($id);
		$obsazeno = DBNabidka::getNabidkaItemLessons($id);
	}
}
//TODO: Spojit R,N-detail a R,N-edit

//-----Dorovnávání skutečného a nastaveného počtu hodin-----//
if(getPostField("pocet_hod") > $data["n_pocet_hod"]) {
	if(getPostField("pocet_hod") < $obsazeno) {
		$_POST["pocet_hod"] = $obsazeno;
	}
	DBNabidka::editNabidka($id, $data["n_trener"], getPostField("pocet_hod"),
		$data["n_od"], $data["n_do"], $data['n_visible'], ($data["n_lock"]) ? 1 : 0);
	$data = DBNabidka::getSingleNabidka($id);
}

include("files/Admin/NabidkaDetail/Display.inc");
return;
?>