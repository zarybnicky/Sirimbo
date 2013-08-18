<?php
include_once('files/Controller/Admin/Akce.php');
class Controller_Admin_Akce_Dokumenty extends Controller_Admin_Akce {
	function __construct() {
		Permissions::checkError('akce', P_OWNED);
	}
	function view($id = null) {
		if(!$id || !($akce = DBAkce::getSingleAkce($id)))
			View::redirect('/admin/akce', 'Akce s takovým ID neexistuje');
		
		header_main("Správa akcí");
		notice($this->redirect()->getRedirectMessage());
		
		$doku = unserialize($akce["a_dokumenty"]);
		
		if(empty($_POST)) {
			include("files/Admin/AkceDokumenty/Display.inc");
			return;
		}
		if(post("remove") !== null) {
			unset($doku[post('remove')]);
			$doku = array_values($doku);
			$changed = true;
		}
		
		if(post("add-id") && DBDokumenty::getSingleDokument(post("add-id"))) {
			$doku[] = post("add-id");
			post('add-id', 0);
			$changed = true;
		}
		if(isset($changed) && $changed) {
			DBAkce::editAkce($akce["a_id"], $akce["a_jmeno"], $akce["a_kde"], $akce["a_info"], $akce["a_od"], $akce["a_do"],
				$akce["a_kapacita"], serialize($doku), $akce["a_lock"], $akce['a_visible']);
			$akce = DBAkce::getSingleAkce($id);
		}
		
		include("files/Admin/AkceDokumenty/Display.inc");
	}
}
?>