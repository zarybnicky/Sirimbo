<?php
User::checkPermissionsError(L_TRENER);

if(!is_numeric(getGetField("id")) && getGetField("id") > 0 && getGetField("id") < 2147483647)
	View::viewError(ER_CORRUPT_DATA);
if(!($nab = DBNabidka::getSingleNabidka(getGetField("id"))))
	View::viewError(ER_CORRUPT_DATA);
$items = DBNabidka::getNabidkaItem(getGetField("id"));
$obsazeno = DBNabidka::getNabidkaItemLessons(getGetField("id"));
$users = DBUser::getUsers();

header_main("Správa nabídek");

if(empty($_POST)) {
	include("files/Admin/NabidkaDetail/Display.inc");
	return;
}

if(getPostField("remove") > 0) {
	DBNabidka::removeNabidkaItem(getGetField("id"), getPostField(getPostField("remove") . "-partner"));
	$items = DBNabidka::getNabidkaItem(getGetField("id"));
	$obsazeno = DBNabidka::getNabidkaItemLessons(getGetField("id"));
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
$items = DBNabidka::getNabidkaItem(getGetField("id"));
$obsazeno = DBNabidka::getNabidkaItemLessons(getGetField("id"));

if(is_numeric(getPostField("add_hodiny")) && is_numeric(getPostField("add_partner"))) {
	$hodiny = getPostField("add_hodiny");
	$partner = getPostField("add_partner");
	
	if(is_numeric($hodiny) && is_numeric($partner)) {
		if(($obsazeno + $hodiny) > getPostField("pocet_hod")) {
			$_POST["pocet_hod"] = $obsazeno + $hodiny;
		}
		
		DBNabidka::addNabidkaItemLessons(getPostField("add_partner"),getGetField("id"),
			getPostField("add_hodiny"));
		unset($_POST["add_partner"]);
		$items = DBNabidka::getNabidkaItem(getGetField("id"));
		$obsazeno = DBNabidka::getNabidkaItemLessons(getGetField("id"));
	}
}

if(getPostField("pocet_hod") != $nab["n_pocet_hod"]) {
	if(getPostField("pocet_hod") < $obsazeno) {
		$_POST["pocet_hod"] = $obsazeno;
	}
	DBNabidka::editNabidka(getGetField("id"), $nab["n_trener"], getPostField("pocet_hod"),
		$nab["n_od"], $nab["n_do"], ($nab["n_lock"]) ? 1 : 0);
	$nab = DBNabidka::getSingleNabidka(getGetField("id"));
}

include("files/Admin/NabidkaDetail/Display.inc");
return;
?>