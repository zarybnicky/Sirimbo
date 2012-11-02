<?php
User::checkPermissionsError(L_ADMIN);
header_main("Správa akcí");

notice(View::getRedirectMessage());

$id = Request::getID();
if(!$id || !is_numeric($id) || !($akce = DBAkce::getSingleAkce($id)))
	View::redirect('/admin/akce', 'Akce s takovým ID neexistuje');

$doku = unserialize($akce["a_dokumenty"]);

if(empty($_POST)) {
	include("files/Admin/AkceDokumenty/Display.inc");
	return;
}

if(post("remove") > 0) {
	$to_remove = array_keys($doku, post("remove"));
	
	if(is_array($to_remove)) {
		foreach($to_remove as $key) {
			unset($doku[$key]);
		}
		$doku = array_values($doku);
	}
	
	DBAkce::editAkce($akce["a_id"], $akce["a_jmeno"], $akce["a_kde"], $akce["a_info"], $akce["a_od"], $akce["a_do"],
		$akce["a_kapacita"], serialize($doku), $akce["a_lock"]);
}

if(post("add-id")) {
	if(DBDokumenty::getSingleDokument(post("add-id"))) {
		$doku[] = post("add-id");
	
		DBAkce::editAkce($akce["a_id"], $akce["a_jmeno"], $akce["a_kde"], $akce["a_info"], $akce["a_od"], $akce["a_do"],
			$akce["a_kapacita"], serialize($doku), $akce["a_kapacita"]);
		
		post('add-id', null);
		$akce = DBAkce::getSingleAkce($id);
		$doku = unserialize($akce["a_dokumenty"]);
	}
}

include("files/Admin/AkceDokumenty/Display.inc");
return;
?>