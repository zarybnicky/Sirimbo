<?php
class Controller_Fotogalerie extends Controller_Abstract {
	function view($id = null) {
		if($id === null) {
			$id = 0;
			$data = DBGalerie::getSingleDir(0);
		} elseif(!($data = DBGalerie::getSingleDir($id))) {
			View::redirect('/fotogalerie', 'Taková složka neexistuje');
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
	}
	function foto($id = null) {
		if(!$id || !($data = DBGalerie::getSingleFoto($id)))
			View::redirect('/fotogalerie', 'Taková fotka neexistuje');
		
		$parent_dir = DBGalerie::getFotky($data['gf_id_rodic']);
		foreach($parent_dir as $key => $foto) {
			if($foto['gf_id'] == $id) {
				$current = $key;
				break;
			}
		}
		$return_url = '/fotogalerie' . ($data['gf_id_rodic'] > 0 ? ('/' . $data['gf_id_rodic']) : '');
		
		echo '<div style="text-align:center;">', $data['gf_name'], '<br/>';
		
		if(isset($parent_dir[$key - 1]))
			echo '<a href="', $parent_dir[$key - 1]['gf_id'], '">&lt;&lt;</a>';
		else
			echo '&lt;&lt;';
		
		echo ' &bull; <a href="', $return_url, '">Zpět</a> &bull; ';
		
		if(isset($parent_dir[$key + 1]))
			echo '<a href="', $parent_dir[$key + 1]['gf_id'], '">&gt;&gt;</a>';
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
	function sidebar() {
		$dirs = DBGalerie::getDirs(true, true);
		
		if(empty($dirs))
			return;
		
		$s = new Sidebar();
		echo $s->menuHeader();
		unset($s);
		
		echo '<ul class="fotoroot" style="padding-top:5px;"><li>';
		
		$level_prev = 0;
		foreach($dirs as $dir) {
			if($dir['gd_hidden'] == '1')
				continue;
			
			if($dir['gd_level'] > $level_prev) {
				echo '<ul class="fotolist">';
			} elseif($dir['gd_level'] == $level_prev) {
				echo '</li>';
			} else {
				for($i = 0; $i < ($level_prev - $dir['gd_level']); $i++)
					echo '</li></ul>';
				echo '</li>';
			}
			if($dir['gd_id'] == 0)
				$link = "/fotogalerie";
			else
				$link = "/fotogalerie/" . $dir['gd_id'];
			
			if($dir['gd_id'] == Request::getID())
				echo '<li><a class="current" href="', $link, '">';
			else
				echo '<li><a href="', $link, '">';
			
			echo '<img src="/style/directory.png" alt="" />', $dir['gd_name'], '</a>';
			$level_prev = $dir['gd_level'];
		}
		for($i = 0; $i < $level_prev; $i++)
			echo '</li></ul>';
		
		echo '</li></ul>';
	}
}