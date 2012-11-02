<?php
User::checkPermissionsError(L_ADMIN);
header_main("Správa akcí");

notice(View::getRedirectMessage());

$a_id = Request::getID();

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
	$user = post($id . '-user');
	$data = DBUser::getUserData($user);
	
	if($user != $item["ai_user"] ||
			($user == "none" ^ $item["ai_user"] == 0)) {
		
		DBAkce::editAkceItem($id, $user, array_shift(explode('-', $data['u_narozeni'])));
	}
}
$items = DBAkce::getAkceItems($a_id);

if(post("add-user") && post("add-rok")) {
	$user = post("add-user");
	$data = DBUser::getUserData($user);
	
	DBAkce::addAkceItem($a_id, $user, array_shift(explode('-', $data['u_narozeni'])));
	post('add-user', null);
	$items = DBAkce::getAkceItems($a_id);
}

include("files/Admin/AkceDetail/Display.inc");
return;
?>