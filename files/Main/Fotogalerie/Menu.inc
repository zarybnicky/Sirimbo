<?php
function getSideMenu() {
	$dirs = DBGalerie::getDirs();
	$menu = array();
	
	if(!empty($dirs)) {
		foreach($dirs as $item) {
			if($item['gd_level'] == 2) {
				$menu[$item['gd_id']] = $item['gd_name'];
			}
		}
	}
	
	return $menu;
}
?>