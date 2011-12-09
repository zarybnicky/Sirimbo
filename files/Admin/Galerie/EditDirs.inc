<?php
$dirs = DBGalerie::getDirs(true);

$parent_dir = array('gd_id' => -1);
foreach($dirs as $dir) {
	if($dir['gd_id'] != $parent_dir['gd_id'])
		$parent_dir = array_k
}
?>