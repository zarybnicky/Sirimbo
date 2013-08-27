<?php
class Controller_Member_Download extends Controller_Abstract {
	function __construct() {
		Permissions::checkError('dokumenty', P_VIEW);
	}
	function view($id = null) {
		if(!get('id'))
			$this->redirect('/member/dokumenty');
		
		$data = DBDokumenty::getSingleDokument(get('id'));
		$path = $data["d_path"];
		$fileName = $data["d_filename"];
		$file = fopen($path, "rb");
	
		header('Pragma: no-cache');
		header('Content-Type: application/octet-stream');
		header('Content-Disposition: inline; filename="' . ($fileName) . '"');
		fpassthru($file);
		fclose($file);
	}
}
?>