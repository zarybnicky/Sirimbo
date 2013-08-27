<?php
include_once('files/Controller/Admin.php');
class Controller_Admin_Novinky extends Controller_Admin {
	function __construct() {
		Permissions::checkError('nastenka', P_OWNED);
	}
	function view($id = null) {
		if(!is_numeric(get('id')))
			$this->redirect("/member/home", "Novinka s daným ID neexistuje");
		DBNovinky::removeNovinka(get('id'));
		$this->redirect("/member/home", "Novinka odstraněna");
	}
}
?>