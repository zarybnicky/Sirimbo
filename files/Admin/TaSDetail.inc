<?php
User::checkPermissionsError(L_ADMIN);

if(!is_numeric(getGetField("id")) && getGetField("id") > 0 && getGetField("id") < 2147483647)
	View::viewError(ER_CORRUPT_DATA);
if(!($tas = DBTaS::getSingleTaS(getGetField("id"))))
	View::viewError(ER_CORRUPT_DATA);
$items = DBTaS::getTaSItems(getGetField("id"));
$users = DBUser::getUsers();

header_main("Správa TaS");

if(empty($_POST)) {
	include("files/Admin/TaSDetail/Display.inc");
	return;
}

if(getPostField("remove") > 0) {
	DBTaS::removeTaSItem(getPostField("remove"));
	$items = DBTaS::getTaSItem(getGetField("id"));
}

foreach($items as $item) {
	$id = $item["ti_id"];
	if((getPostField($id . "-reg") && $item["ti_user"] == NULL &&
			getPostField($id . "-user") > 0) ||
			(!getPostField($id . "-reg") && $item["ti_user"] != NULL) ||
			!(getPostField($id . "-user") == $item["ti_user"] ||
			(getPostField($id . "-user") == "none" && $item["ti_user"] == NULL)) ||
			getPostField($id . "-jmeno") != $item["ti_jmeno"] ||
			getPostField($id . "-prijmeni") != $item["ti_prijmeni"] ||
			getPostField($id . "-rok") != $item["ti_rok_narozeni"]) {
		if(!getPostField($id . "-reg"))
			$_POST[$id . "-user"] = NULL;
		
		DBTaS::editTaSItem($id, getPostField($id . "-user"), getPostField($id . "-jmeno"),
			getPostField($id . "-prijmeni"), getPostField($id . "-rok"));
	}
}
$items = DBTaS::getTaSItems(getGetField("id"));

if(getPostField("add-jmeno") && getPostField("add-prijmeni") && getPostField("add-rok")) {
	$user = (getPostField("add-reg")) ? getPostField("add-user") : NULL;
	$jmeno = getPostField("add-jmeno");
	$prijmeni = getPostField("add-prijmeni");
	$rok = getPostField("add-rok");
	
	if(preg_match("/^19[0-9]{2}|20[0,1,2,3][0-9]$/", $rok)) {
		DBTaS::addTaSItem(getGetField("id"), $user, $jmeno, $prijmeni, $rok);
		unset($_POST["add-user"]);
		$items = DBTaS::getTaSItems(getGetField("id"));
	}
}

include("files/Admin/TaSDetail/Display.inc");
return;
?>