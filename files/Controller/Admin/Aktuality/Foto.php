<?php
class Controller_Admin_Aktuality_Foto implements Controller_Interface {
	function __construct() {
		Permissions::checkError('aktuality', P_OWNED);
	}
	function view($id = null) {
		
	}
}
?>