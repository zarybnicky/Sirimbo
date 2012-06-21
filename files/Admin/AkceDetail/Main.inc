<?php
User::checkPermissionsError(L_ADMIN);
header_main("Správa akcí");

notice(View::getRedirectMessage());

$request = explode('/', $_SERVER['REQUEST_URI']);
$action = array_slice($request, 3);
$a_id = $action[1];

if(!$a_id || !is_numeric($a_id) || !($akce = DBAkce::getSingleAkce($a_id)))
	View::redirect('/admin/akce', 'Akce s takovým ID neexistuje');

$items = DBAkce::getAkceItems($a_id);
$users = DBUser::getActiveUsers();

if(empty($_POST)) {
	include("files/Admin/AkceDetail/Display.inc");
	return;
}

if(post("remove") > 0) {
	DBAkce::removeAkceItem(post("remove"));
	$items = DBAkce::getAkceItems($a_id);
}

foreach($items as $item) {
	$id = $item["ai_id"];
	
	if(post($id . "-user") != $item["ai_user"] ||
			(post($id . "-user") == "none" ^ $item["ai_user"] == 0) ||
			post($id . "-rok") != $item["ai_rok_narozeni"]) {
		
		DBAkce::editAkceItem($id, post($id . "-user"), post($id . "-rok"));
	}
}
$items = DBAkce::getAkceItems($a_id);

if(post("add-user") && post("add-rok")) {
	$user = post("add-user");
	$rok = post("add-rok");
	
	if(preg_match("/^19[0-9]{2}|20[0,1,2,3][0-9]$/", $rok)) {
		DBAkce::addAkceItem($a_id, $user, $rok);
		post('add-user', null);
		$items = DBAkce::getAkceItems($a_id);
	}
}

include("files/Admin/AkceDetail/Display.inc");
return;
?>