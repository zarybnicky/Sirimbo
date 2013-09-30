<?php
include_once('files/Controller/Admin/Platby.php');
class Controller_Admin_Platby_Overview extends Controller_Admin_Platby {
	function __construct() {
		Permissions::checkError('platby', P_OWNED);
	}
	function view($id = null) {
		$this->render('files/View/Admin/Platby/Navigation.inc');
	}
}