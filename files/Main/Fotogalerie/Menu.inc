<?php
function getSideMenu() {
	$dirs = DBGalerie::getDirs(true, true);
	$dirs[] = array('gd_level' => 2, 'gd_id' => '#', 'gd_name' => 'Další');
	
	if(empty($dirs))
		return;
	
	$level_prev = 0;
	foreach($dirs as $dir) {
		if($dir['gd_level'] > $level_prev) {
			if($level_prev != 0)
				echo '</a>';
			echo '<ul class="fotolist">';
			echo '<li><a href="', $dir['gd_id'], '"><img src="/style/directory.png" alt="" />', $dir['gd_name'];
		} elseif($dir['gd_level'] == $level_prev) {
			echo '</a></li>';
			echo '<li><a href="', $dir['gd_id'], '"><img src="/style/directory.png" alt="" />', $dir['gd_name'];
		} else {
			for($i = 0; $i < ($level_prev - $dir['gd_level']); $i++)
				echo '</a></li></ul>';
			echo '</a></li>';
			echo '<li><a href="', $dir['gd_id'], '"><img src="/style/directory.png" alt="" />', $dir['gd_name'];
		}
		$level_prev = $dir['gd_level'];
	}
	for($i = 0; $i < $level_prev; $i++)
		echo '</a></li></ul>';
/*
	foreach($dirs as $item) {
		if($item['gd_level'] == 2) {
			$menu[$item['gd_id']] = $item['gd_name'];
		}
	}
*/
}
?>