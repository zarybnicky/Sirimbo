<?php
header_main('Fotogalerie');
notice(View::getRedirectMessage());

if(Request::getAction() == 'foto') {
	$id = Request::getID();
	if(!$id || !($data = DBGalerie::getSingleFoto($id)))
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
	
	$return_url = '/fotogalerie' . ($data['gf_id_rodic'] > 0 ? ('/' . $data['gf_id_rodic']) : '');
	
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
	
	echo '<a href="', $return_url, '">';
	echo '<div class="f_full"><div class="f_img">';
	echo '<img alt="', $data['gf_id'], '" src="',
		str_replace('./galerie/', '/galerie/', $data['gf_path']), '" />';
	echo '</div></div>';
	echo '</a>';
	return;
}

if(Request::getID() === null) {
	$id = 0;
	$data = DBGalerie::getSingleDir(0);
} elseif(!($data = DBGalerie::getSingleDir(Request::getID()))) {
	View::redirect('/fotogalerie', 'Taková složka neexistuje');
} else {
	$id = Request::getID();
}
$fotos = DBGalerie::getFotky($id);

header_minor($data['gd_name']);
if(empty($fotos)) {
	notice('Žádné fotky');
	return;
}
foreach($fotos as $item) {
	$tn = str_replace('./galerie', '/galerie/thumbnails', $item['gf_path']);
	
	echo '<a href="', $_SERVER['REQUEST_URI'], '/foto/', $item['gf_id'], '" class="f_preview">';
	echo '<div class="f_img">';
	echo '<img alt="', $item['gf_id'], '" src="', $tn, '" />';
	echo '</div></a>';
}
?>
