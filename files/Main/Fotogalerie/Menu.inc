<?php
function getSideMenu() {
	$dirs = DBGalerie::getDirs(true, true);
	
	if(empty($dirs))
		return;
	
	echo '<ul class="fotoroot"><li>';
	
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
		
		echo '<li><a href="', $link, '"><img src="/style/directory.png" alt="" />', $dir['gd_name'], '</a>';
		$level_prev = $dir['gd_level'];
	}
	for($i = 0; $i < $level_prev; $i++)
		echo '</li></ul>';
	
	echo '</li></ul>';
/*
	foreach($dirs as $item) {
		if($item['gd_level'] == 2) {
			$menu[$item['gd_id']] = $item['gd_name'];
		}
	}
*/
}
?>