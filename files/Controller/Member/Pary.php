<?php
class Controller_Member_Pary implements Controller_Interface {
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