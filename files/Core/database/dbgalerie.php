<?php
class DBGalerie extends Database {
	public static function getFotky() {
		$res = DBGalerie::query('SELECT * FROM galerie_foto');
		
		return DBGalerie::getArray($res);
	}
	public static function getDirs($by_parent = false) {
		$res = DBGalerie::query('SELECT * FROM galerie_dir' . ($by_parent ? ' ORDER BY gd_id_rodic' : ''));
		
		return DBGalerie::getArray($res);
	}
	public static function getSingleDir($id) {
		list($id) = DBGalerie::escapeArray(array($id));
		
		$res = DBGalerie::query("SELECT * FROM galerie_dir WHERE gd_id='$id'");
		return DBGalerie::getSingleRow($res);
	}
	public static function addFoto($dir, $path, $name, $kdo) {
		list($dir, $path, $name, $kdo) = DBGalerie::escapeArray(array($dir, $path, $name, $kdo));
		
		DBGalerie::query("INSERT INTO galerie_foto (gf_id_rodic,gf_path,gf_name,gf_kdo) VALUES " .
			"('$dir','$path','$name','$kdo')");
		return true;
	}
}
?>