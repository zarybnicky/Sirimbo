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
	
	$subdirs = DBGalerie::getSubdirs($id);
	$fotos = DBGalerie::getFotky($id);
	
	header_minor($data['gd_name']);
	if(empty($fotos) && empty($subdirs)) {
		notice('Žádné fotky');
		return;
	}
	foreach($subdirs as $item) {
		echo '<div class="f"><a href="', $_SERVER['REQUEST_URI'], '/', $item['gd_id'], '">';
		echo '<div class="f_img"><div class="f_preview">';
		echo '<img alt="', $item['gd_id'], '" src="/images/folder.png" />';
		echo '</div></div>';
		echo '<div class="f_popis">', $item['gd_name'], '</div>';
		echo '</a></div>';
	}
	foreach($fotos as $item) {
		$tn = str_replace('./galerie', '/galerie/thumbnails', $item['gf_path']);
		
		echo '<div class="f"><a href="', $_SERVER['REQUEST_URI'], '/foto/', $item['gf_id'], '">';
		echo '<div class="f_img"><div class="f_preview">';
		echo '<img alt="', $item['gf_id'], '" src="', $tn, '" />';
		echo '</div></div>';
		echo '<div class="f_popis">', $item['gf_name'], '</div>';
		echo '</a></div>';
	}
} else {
	//Foto
	$id = is_numeric($url[0]) ? $url[0] : 0;
	if(!($data = DBGalerie::getSingleFoto($id)))
		View::redirect('/fotogalerie', 'Taková fotka neexistuje');
	
	$return_url = implode('/', array_reverse(array_slice($url, 2)));
	
	echo '<a href="', $return_url, '">';
	echo '<div class="f_img"><div class="f_full">';
	echo '<img alt="', $data['gf_id'], '" src="',
		str_replace('./galerie/', '/galerie/', $data['gf_path']), '" /><br/>';
	echo '</div></div>';
	echo '</a>';
	
	echo '<div style="text-align:center;">', $data['gf_name'], ' &bull; <a href="', $return_url, '">Zpět</a></div>';
}
?>