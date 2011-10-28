<?php
User::checkPermissionsError(L_ADMIN);

$request = explode('/', $_SERVER['REQUEST_URI']);
$action = array_slice($request, 3);
$t_id = $action[1];

if(!is_numeric($t_id) || ($t_id < 0 || $t_id > 2147483647))
	View::viewError(ER_CORRUPT_DATA);
if(!($tas = DBTaS::getSingleTaS($t_id)))
	View::viewError(ER_CORRUPT_DATA);
$items = DBTaS::getTaSItems($t_id);
$users = DBUser::getActiveUsers();

header_main("Správa TaS");

if(empty($_POST)) {
	include("files/Admin/TaSDetail/Display.inc");
	return;
}

if(getPostField("remove") > 0) {
	DBTaS::removeTaSItem(getPostField("remove"));
	$items = DBTaS::getTaSItems($t_id);
}

foreach($items as $item) {
	$id = $item["ti_id"];
	
	if(getPostField($id . "-user") != $item["ti_user"] ||
			(getPostField($id . "-user") == "none" ^ $item["ti_user"] == 0) ||
			getPostField($id . "-jmeno") != $item["ti_jmeno"] ||
			getPostField($id . "-prijmeni") != $item["ti_prijmeni"] ||
			getPostField($id . "-rok") != $item["ti_rok_narozeni"]) {
		if(!getPostField($id . "-reg")) {
			$_POST[$id . "-user"] = 0;
		} elseif(is_numeric(getPostField($id . "-user"))) {
			$u_data = DBUser::getUserData(getPostField($id . '-user'));
			
			$_POST[$id . '-jmeno'] = $u_data['u_jmeno'];
			$_POST[$id . '-prijmeni'] = $u_data['u_prijmeni'];
		}
		
		DBTaS::editTaSItem($id, getPostField($id . "-user"), getPostField($id . "-jmeno"),
			getPostField($id . "-prijmeni"), getPostField($id . "-rok"));
	}
}
$items = DBTaS::getTaSItems($t_id);

if((getPostField("add-reg") && getPostField("add-user")) ||
		(getPostField("add-jmeno") && getPostField("add-prijmeni")) && getPostField("add-rok")) {
	$user = (getPostField("add-reg")) ? getPostField("add-user") : 0;
	$jmeno = getPostField("add-jmeno");
	$prijmeni = getPostField("add-prijmeni");
	$rok = getPostField("add-rok");
	
	if(!$jmeno && !$prijmeni) {
		$u_data = DBUser::getUserData($user);
		$jmeno = $u_data['u_jmeno'];
		$prijmeni = $u_data['u_prijmeni'];
	}
	
	if(preg_match("/^19[0-9]{2}|20[0,1,2,3][0-9]$/", $rok)) {
		DBTaS::addTaSItem($t_id, $user, $jmeno, $prijmeni, $rok);
		unset($_POST["add-user"]);
		$items = DBTaS::getTaSItems($t_id);
	}
}

include("files/Admin/TaSDetail/Display.inc");
return;
?>