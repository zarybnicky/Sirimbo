<?php
header_main('Fotogalerie');
notice(View::getRedirectMessage());

$dirs = DBGalerie::getDirs(true, true);
$pieces = explode('/', $_SERVER['REQUEST_URI']);
$id_foto = array_pop($pieces);
$id_dir = array_pop($pieces);
if(!is_numeric($id_dir) && $id_dir != 'foto') {
	//Ve složce
	$id = is_numeric($id_foto) ? $id_foto : 0;
	if(!($dir_data = DBGalerie::getSingleDir($id)))
		View::redirect('/galerie', 'Taková složka neexistuje');
	$fotos = DBGalerie::getFotky($id);
	
	header_minor($dir_data);
}
?>