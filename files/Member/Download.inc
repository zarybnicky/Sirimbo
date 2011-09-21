<?php
User::checkPermissionsError(L_USER);

if(!empty($_GET["id"])) {
	$data = DBDokumenty::getSingleDokument($_GET["id"]);
	$path = $data["d_path"];
	$fileName = $data["d_filename"];
	$file = fopen($path, "rb");
	var_dump(array($data, $file));

	header('Pragma: no-cache');
	header('Content-Type: application/octet-stream');
	header('Content-Disposition: inline; filename='. $fileName);
	fpassthru($file);
	return;
}
?>
Co tu děláš?? Bež pryč, tady nic není :o)