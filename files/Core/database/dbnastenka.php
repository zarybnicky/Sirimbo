<?php
class DBNastenka extends Database {
	public static function getNastenka() {
		$res = DBNastenka::query("SELECT up_id,u_login,u_jmeno,u_prijmeni,up_aktu,up_nadpis,up_text,up_barvy,up_lock" .
			" FROM upozorneni LEFT JOIN users ON up_kdo=u_id");
		return DBNastenka::getArray($res);
	}
	
	public static function getNastenkaUserName($id) {
		list($id) = DBNastenka::escapeArray(array($id));
		
		$res = DBNastenka::query("SELECT u_login FROM upozorneni LEFT JOIN users ON up_kdo=u_id WHERE " .
			"up_id='$id'");
		if(!$res) {
			return false;
		} else {
			$row = DBNastenka::getSingleRow($res);
			return $row["u_login"];
		}
	}
	
	public static function getSingleNastenka($id) {
		list($id) = DBNastenka::escapeArray(array($id));
		
		$res = DBNastenka::query("SELECT u_login,u_jmeno,u_prijmeni,up_aktu,up_nadpis,up_text,up_barvy,up_lock" .
			" FROM upozorneni LEFT JOIN users ON up_kdo=u_id WHERE up_id='$id'");
		if(!$res) {
			return false;
		} else {
			return DBNastenka::getSingleRow($res);
		}
	}
	
	public static function isNastenkaLocked($id) {
		list($id) = DBNastenka::escapeArray(array($id));
		
		$res = DBNastenka::query("SELECT up_lock FROM upozorneni WHERE up_id='$id'");
		if(!$res) {
			return false;
		} else {
			$row = DBNastenka::getSingleRow($res);
			return $row["up_lock"];
		}
	}
	
	public static function addNastenka($userid, $nadpis, $text, $barvy, $lock) {
		list($userid, $nadpis, $text, $barvy, $lock) =
			DBNastenka::escapeArray(array($userid, $nadpis, $text, $barvy, $lock));
		
		DBNastenka::query("INSERT INTO upozorneni (up_kdo,up_nadpis,up_text,up_barvy,up_lock) VALUES " .
			"('$userid','$nadpis','$text','$barvy','$lock')");
		
		return true;
	}
	
	public static function editNastenka($id, $nadpis, $text, $barvy, $lock) {
		list($id, $nadpis, $text, $barvy, $lock) =
			DBNastenka::escapeArray(array($id, $nadpis, $text, $barvy, $lock));
		
		DBNastenka::query("UPDATE upozorneni SET " .
			"up_nadpis='$nadpis',up_text='$text',up_barvy='$barvy',up_lock='$lock' WHERE up_id='$id'");
		
		return true;
	}

	public static function removeNastenka($id) {
		list($id) = DBNastenka::escapeArray(array($id));
		
		DBNastenka::query("DELETE FROM upozorneni WHERE up_id='$id'");
		
		return true;
	}
}
?>