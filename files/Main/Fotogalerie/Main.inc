<?php
header_main('Fotogalerie');
notice(View::getRedirectMessage());

$dirs = DBGalerie::getDirs(true, true);
$url = explode('/', $_SERVER['REQUEST_URI']);
$url = array_reverse($url);

if($url[1] != 'foto') {
	//Ve složce
	$id = is_numeric($url[0]) ? $url[0] : 0;
	if(!($data = DBGalerie::getSingleDir($id)))
		View::redirect('/fotogalerie', 'Taková složka neexistuje');
	$fotos = DBGalerie::getFotky($id);
	
	header_minor($data['gd_name']);
	if(empty($fotos)) {
		notice('Žádné fotky');
		return;
	}
	foreach($fotos as $item) {
		$pieces = explode('/', $item['gf_path']);
		$pieces[2] = 'tn_' . $pieces[2];
		$tn = '/' . implode('/', $pieces);
		
		echo '<div class="f"><a href="', $_SERVER['REQUEST_URI'], '/foto/', $item['gf_id'], '">';
		echo '<div class="f_img"><div class="f_preview">';
		echo '<img alt="', $item['gf_id'], '" src="', $tn, '" />';
		echo '</div></div>';
		echo '<div class="f_popis">', $item['gf_name'], '</div>';
		echo '</div></a>';
	}
} else {
	//Foto
	$id = is_numeric($url[0]) ? $url[0] : 0;
	if(!($data = DBGalerie::getSingleFoto($id)))
		View::redirect('/fotogalerie', 'Taková fotka neexistuje');
	
	$return_url = implode('/', array_reverse(array_slice($url, 2)));
	
	echo '<a href="', $return_url, '">';
	echo '<div class="f_img"><div class="f_full">';
	echo '<img alt="', $data['gf_id'], '" src="/', $data['gf_path'], '" /><br/>';
	echo '</div></div>';
	echo '</a>';
	
	echo '<div style="text-align:center;">', $data['gf_name'], ' &bull; <a href="', $return_url, '">Zpět</a></div>';
}
?>