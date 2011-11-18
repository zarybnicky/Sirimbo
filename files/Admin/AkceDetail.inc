<?php
User::checkPermissionsError(L_ADMIN);
header_main("Správa akcí");

notice(View::getRedirectMessage());

$request = explode('/', $_SERVER['REQUEST_URI']);
$action = array_slice($request, 3);
$a_id = $action[1];

if(!a_id || !is_numeric($a_id) || !($akce = DBAkce::getSingleAkce($a_id)))
	View::redirect('/admin/akce', 'Akce s takovým ID neexistuje');

$items = DBAkce::getAkceItems($a_id);
$users = DBUser::getActiveUsers();

if(empty($_POST)) {
	include("files/Admin/AkceDetail/Display.inc");
	return;
}

if(getPostField("remove") > 0) {
	DBAkce::removeAkceItem(getPostField("remove"));
	$items = DBAkce::getAkceItems($a_id);
}

foreach($items as $item) {
	$id = $item["ai_id"];
	
	if(getPostField($id . "-user") != $item["ai_user"] ||
			(getPostField($id . "-user") == "none" ^ $item["ai_user"] == 0) ||
			getPostField($id . "-jmeno") != $item["ai_jmeno"] ||
			getPostField($id . "-prijmeni") != $item["ai_prijmeni"] ||
			getPostField($id . "-rok") != $item["ai_rok_narozeni"]) {
		if(!getPostField($id . "-reg")) {
			$_POST[$id . "-user"] = 0;
		} elseif(is_numeric(getPostField($id . "-user"))) {
			$u_data = DBUser::getUserData(getPostField($id . '-user'));
			
			$_POST[$id . '-jmeno'] = $u_data['u_jmeno'];
			$_POST[$id . '-prijmeni'] = $u_data['u_prijmeni'];
		}
		
		DBAkce::editAkceItem($id, getPostField($id . "-user"), getPostField($id . "-jmeno"),
			getPostField($id . "-prijmeni"), getPostField($id . "-rok"));
	}
}
$items = DBAkce::getAkceItems($a_id);

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
		DBAkce::addAkceItem($a_id, $user, $jmeno, $prijmeni, $rok);
		unset($_POST["add-user"]);
		$items = DBAkce::getAkceItems($a_id);
	}
}

include("files/Admin/AkceDetail/Display.inc");
return;
?>