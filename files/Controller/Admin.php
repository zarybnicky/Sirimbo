<?php
class Controller_Admin implements Controller_Interface {
	function __construct() {
		Permissions::checkError('nastenka', P_OWNED);
	}
	function view($id = null) {
		include('files/Admin/Main.inc');
	}
}
?>