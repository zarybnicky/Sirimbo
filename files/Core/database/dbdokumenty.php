<?php
class DBDokumenty extends Database {
	public static function getDokumenty() {
		$res = DBDokumenty::query("SELECT u_jmeno,u_prijmeni,d_id,d_path,d_name,d_filename,d_kategorie,d_kdo" .
			" FROM dokumenty LEFT JOIN users ON d_kdo=u_id");
		return DBDokumenty::getArray($res);
	}
	
	public static function getDokumentyByKategorie($kat) {
		$res = DBDokumenty::query("SELECT u_jmeno,u_prijmeni,d_id,d_path,d_name,d_filename,d_kategorie,d_kdo" .
			" FROM dokumenty LEFT JOIN users ON d_kdo=u_id WHERE d_kategorie='$kat'");
		return DBDokumenty::getArray($res);
	}
	
	public static function getSingleDocument($id) {
		list($id) = DBDokumenty::escapeArray(array($id));
		//TODO: Handle database returning FALSE
		$res = DBDokumenty::query("SELECT u_jmeno,u_prijmeni,d_id,d_path,d_name,d_filename,d_kategorie,d_kdo" .
			" FROM dokumenty LEFT JOIN users ON d_kdo=u_id WHERE d_id='$id'");
		if(!$res) {
			return false;
		} else {
			return DBDokumenty::getSingleRow($res);
		}
	}
	
	public static function getDocumentPath($id) {
		list($id) = DBDokumenty::escapeArray(array($id));
		
		$res = DBDokumenty::query("SELECT d_path FROM dokumenty WHERE d_id='$id'");
		if(!$res) {
			return false;
		} else {
			$row = DBDokumenty::getSingleRow($res);
			return $row["d_path"];
		}
	}
	
	public static function getDokumentUserID($id) {
		list($id) = DBDokumenty::escapeArray(array($id));
		
		$res = DBDokumenty::query("SELECT d_kdo FROM dokumenty WHERE d_id='$id'");
		if(!$res) {
			return false;
		} else {
			$row = DBDokumenty::getSingleRow($res);
			return $row["d_kdo"];
		}
	}
	
	public static function addDokument($path, $name, $filename, $kategorie, $kdo) {
		list($path, $kategorie, $kdo) = DBDokumenty::escapeArray(array($path, $kategorie, $kdo));
		
		DBDokumenty::query("INSERT INTO dokumenty (d_path,d_name,d_filename,d_kategorie,d_kdo) VALUES " .
			"('$path','$name','$filename','$kategorie','$kdo')");
		return true;
	}
	
	public static function editDokument($id, $newname) {
		list($id, $newname) = DBDokumenty::escapeArray(array($id, $newname));
		
		DBDokumenty::query("UPDATE dokumenty SET d_name='$newname' WHERE d_id='$id'");
		
		return true;
	}
	
	public static function removeDokument($id) {
		list($id) = DBDokumenty::escapeArray(array($id));
		
		DBDokumenty::query("DELETE FROM dokumenty WHERE d_id='$id'");
		
		return true;
	}
}
?>