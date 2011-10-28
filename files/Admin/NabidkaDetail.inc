<?php
User::checkPermissionsError(L_TRENER);

$request = explode('/', $_SERVER['REQUEST_URI']);
$action = array_slice($request, 3);
$id = $action[1];

if(!is_numeric($id) || ($id < 0 || $id > 2147483647))
	View::viewError(ER_CORRUPT_DATA);
if(!($nab = DBNabidka::getSingleNabidka($id)))
	View::viewError(ER_CORRUPT_DATA);

$items = DBNabidka::getNabidkaItem($id);
$obsazeno = DBNabidka::getNabidkaItemLessons($id);
$users = DBPary::getPartners();

header_main("Správa nabídek");

if(empty($_POST)) {
	include("files/Admin/NabidkaDetail/Display.inc");
	return;
}

if(getPostField("remove") > 0) {
	DBNabidka::removeNabidkaItem($id, getPostField(getPostField("remove") . "-partner"));
	$items = DBNabidka::getNabidkaItem($id);
	$obsazeno = DBNabidka::getNabidkaItemLessons($id);
}

foreach($items as $item) {
	if(getPostField($item["ni_id"] . "-partner") != $item["ni_partner"] ||
			getPostField($item["ni_id"] . "-hodiny") != $item["ni_pocet_hod"]) {
		$rozdil_hod = getPostField($item["ni_id"] . "-hodiny") - $item["ni_pocet_hod"];
		if(($obsazeno + $rozdil_hod) > $nab["n_pocet_hod"]) {
			$_POST["pocet_hod"] = $obsazeno + $rozdil_hod;
		}
		DBNabidka::editNabidkaItem($item["ni_id"], getPostField($item["ni_id"] . "-partner"),
			getPostField($item["ni_id"] . "-hodiny"));
	}
}
$items = DBNabidka::getNabidkaItem($id);
$obsazeno = DBNabidka::getNabidkaItemLessons($id);

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
//FIXME: redirects with messages ($_SESSION['notice']) - echo in View::viewDynamic()???

if(getPostField("pocet_hod") > $nab["n_pocet_hod"]) {
	if(getPostField("pocet_hod") < $obsazeno) {
		$_POST["pocet_hod"] = $obsazeno;
	}
	DBNabidka::editNabidka($id, $nab["n_trener"], getPostField("pocet_hod"),
		$nab["n_od"], $nab["n_do"], $nab['n_visible'], ($nab["n_lock"]) ? 1 : 0);
	$nab = DBNabidka::getSingleNabidka($id);
}

include("files/Admin/NabidkaDetail/Display.inc");
return;
?>