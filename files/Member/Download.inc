<?php
User::checkPermissionsError(L_HOST);

if(get('id')) {
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
?>
Co tu děláš? Běž pryč, tady nic není :o)