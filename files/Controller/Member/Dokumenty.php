<?php
class Controller_Member_Dokumenty implements Controller_Interface {
	function __construct() {
		Permissions::checkError('dokumenty', P_VIEW);
	}
	function view($id = null) {
		include('files/Member/Dokumenty.inc');
	}
}
?>