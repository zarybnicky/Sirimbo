<?php
class Controller_Member_Nastenka implements Controller_Interface {
	function __construct() {
		Permissions::checkError('dokumenty', P_VIEW);
	}
	function view($id = null) {
		include('files/Member/Nastenka.inc');
	}
}
?>