<?php
User::checkPermissionsError(L_HOST);

if(!empty($_GET["id"])) {
	$data = DBDokumenty::getSingleDokument($_GET["id"]);
	$path = $data["d_path"];
	$fileName = $data["d_filename"];
	$file = fopen($path, "rb");

	header('Pragma: no-cache');
	header('Content-Type: application/octet-stream');
	header('Content-Disposition: inline; filename='. $fileName);
	fpassthru($file);
	return;
}
?>
Co tu děláš? Bež pryč, tady nic není :o)