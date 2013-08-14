<?php
class Controller_Member_Download extends Controller_Abstract {
	function __construct() {
		Permissions::checkError('dokumenty', P_VIEW);
	}
	function view($id = null) {
		if(!get('id')) {
			echo 'Co tu děláš? Běž pryč, tady nic není :o)';
			return;
		}
		ob_clean();
		$data = DBDokumenty::getSingleDokument(get('id'));
		$path = $data["d_path"];
		$fileName = $data["d_filename"];
		$file = fopen($path, "rb");
	
		header('Pragma: no-cache');
		header('Content-Type: application/octet-stream');
		header('Content-Disposition: inline; filename="' . ($fileName) . '"');
		fpassthru($file);
		fclose($file);
		exit;
	}
}
?>