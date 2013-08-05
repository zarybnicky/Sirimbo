<?php
include_once('files/Controller/Member.php');
class Controller_Member_Pary extends Controller_Member {
	function __construct() {
		Permissions::checkError('pary', P_VIEW);
	}
	function view($id = null) {
		if($id) {
			header_main('Detail páru');
	   		DisplayPary::viewFullPar($id);
			return;
		}
		include('files/Member/Pary.inc');
	}
}
?>