<?php
header_main('Fotogalerie');
notice(View::getRedirectMessage());

$url = array_reverse(explode('/', $_SERVER['REQUEST_URI']));

if($url[1] == 'foto') {
	//Foto
	$id = is_numeric($url[0]) ? $url[0] : 0;
	if(!($data = DBGalerie::getSingleFoto($id)))
		View::redirect('/fotogalerie', 'Taková fotka neexistuje');
	
	$parent_dir = DBGalerie::getFotky($data['gf_id_rodic']);
	foreach($parent_dir as $key => $foto)
		if($foto['gf_id'] == $id) {
			$current = $key;
			break;
		}
	if(isset($parent_dir[$key - 1]))
		$id_prev= $parent_dir[$key - 1]['gf_id'];
	else
		$id_prev = false;
	if(isset($parent_dir[$key + 1]))
		$id_next = $parent_dir[$key + 1]['gf_id'];
	else
		$id_next = false;
	
	$return_url = implode('/', array_reverse(array_slice($url, 2)));
	
	echo '<a href="', $return_url, '">';
	echo '<div class="f_img"><div class="f_full">';
	echo '<img alt="', $data['gf_id'], '" src="',
		str_replace('./galerie/', '/galerie/', $data['gf_path']), '" />';
	echo '</div></div>';
	echo '</a>';
	
	echo '<div style="text-align:center;">', $data['gf_name'], '<br/>';
	if($id_prev !== false)
		echo '<a href="', $id_prev, '">&lt;&lt;</a>';
	else
		echo '&lt;&lt;';
	echo ' &bull; <a href="', $return_url, '">Zpět</a> &bull; ';
	if($id_next !== false)
		echo '<a href="', $id_next, '">&gt;&gt;</a>';
	else
		echo '&gt;&gt;';
	echo '</div>';
	return;
}

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
	$tn = str_replace('./galerie', '/galerie/thumbnails', $item['gf_path']);
	
	echo '<div class="f"><a href="', $_SERVER['REQUEST_URI'], '/foto/', $item['gf_id'], '">';
	echo '<div class="f_img"><div class="f_preview">';
	echo '<img alt="', $item['gf_id'], '" src="', $tn, '" />';
	echo '</div></div>';
	echo '<div class="f_popis">', $item['gf_name'], '</div>';
	echo '</a></div>';
}
?>