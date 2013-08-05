<?php
include_once('files/Controller/Member.php');
class Controller_Member_Nastenka extends Controller_Member {
	function __construct() {
		Permissions::checkError('dokumenty', P_VIEW);
	}
	function view($id = null) {
		include('files/Member/Nastenka.inc');
	}
}
?>