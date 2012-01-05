<?php
class DBGalerie extends Database {
	public static function getFotky($dir = false) {
		if($dir !== false)
			list($dir) = DBGalerie::escapeArray(array($dir));
		$res = DBGalerie::query('SELECT * FROM galerie_foto' . ($dir !== false ? " WHERE gf_id_rodic='$dir'" : ''));
		
		return DBGalerie::getArray($res);
	}
	private static function recursiveChildren(&$dirs, &$out, $dirId, $count) {
		if(empty($dirs))
			return;
		
		for($i = 0; $i < $count; $i++) {
			if(!isset($dirs[$i]) || $dirs[$i]['gd_id_rodic'] != $dirId)
				continue;
			$out[] = $dirs[$i];
			$id = $dirs[$i]['gd_id'];
			unset($dirs[$i]);
			DBGalerie::recursiveChildren($dirs, $out, $id, $count);
		}
	}
	public static function getDirs($by_level = false, $sort = false) {
		$res = DBGalerie::query('SELECT * FROM galerie_dir' . ($by_level ? ' ORDER BY gd_level' : ''));
		$array = DBGalerie::getArray($res);
		
		if($sort) {
			$out = array();
			DBGalerie::recursiveChildren($array, $out, 0, count($array));
			$array = $out;
		}
		
		return $array;
	}
	public static function getSingleDir($id) {
		list($id) = DBGalerie::escapeArray(array($id));
		
		$res = DBGalerie::query("SELECT * FROM galerie_dir WHERE gd_id='$id'");
		return DBGalerie::getSingleRow($res);
	}
	public static function getSingleFoto($id) {
		list($id) = DBGalerie::escapeArray(array($id));
		
		$res = DBGalerie::query("SELECT * FROM galerie_foto WHERE gf_id='$id'");
		return DBGalerie::getSingleRow($res);
	}
	public static function addFoto($dir, $path, $name, $kdo) {
		list($dir, $path, $name, $kdo) = DBGalerie::escapeArray(array($dir, $path, $name, $kdo));
		
		DBGalerie::query("INSERT INTO galerie_foto (gf_id_rodic,gf_path,gf_name,gf_kdo) VALUES " .
			"('$dir','$path','$name','$kdo')");
		return true;
	}
	public static function editFoto($id, $path, $dir = false, $name = false) {
		list($id, $path, $dir, $name) = DBNabidka::escapeArray(array($id, $path, $dir, $name));
		
		DBNabidka::query("UPDATE galerie_foto SET gf_path='$path'" .
			($dir != false ? ",gf_id_rodic='$dir'" : '') . ($name != false ? ",gf_name='$name'" : '') .
			" WHERE gf_id='$id'");
		
		return true;
	}
	public static function removeFoto($id) {
		list($id) = DBGalerie::escapeArray(array($id));
		
		DBGalerie::query("DELETE FROM galerie_foto WHERE gf_id='$id'");
		return true;
	}
	public static function addDir($name, $parent, $level) {
		list($name, $parent, $level) = DBGalerie::escapeArray(array($name, $parent, $level));
		
		DBGalerie::query("INSERT INTO galerie_dir (gd_name,gd_id_rodic,gd_level) VALUES " .
			"('$name','$parent','$level')");
		
		return true;
	}
	public static function editNabidka($id, $trener, $pocet_hod, $od, $do, $visible, $lock) {
		list($id, $trener, $pocet_hod, $od, $do, $visible, $lock) =
			DBNabidka::escapeArray(array($id, $trener, $pocet_hod, $od, $do, $visible, $lock));
		
		DBNabidka::query("UPDATE nabidka SET n_trener='$trener',n_pocet_hod='$pocet_hod',n_od='$od'," .
			"n_do='$do',n_visible='$visible',n_lock='$lock' WHERE n_id='$id'");
		
		return true;
	}
	public static function removeDir($id) {
		list($id) = DBGalerie::escapeArray(array($id));
		
		DBGalerie::query("DELETE FROM galerie_dir WHERE gd_id='$id'");
		DBGalerie::query("DELETE FROM galerie_dir WHERE gd_id_rodic='$id'");
		DBGalerie::query("DELETE FROM galerie_foto WHERE gf_id_rodic='$id'");
		
		return true;
	}
}
?>