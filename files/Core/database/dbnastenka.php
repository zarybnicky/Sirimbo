<?php
class DBNastenka extends Database {
	public static function getNastenka() {
		$res = DBNastenka::query("SELECT up_id,u_login,u_jmeno,u_prijmeni,up_aktu,up_nadpis,up_text,up_lock" .
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
		
		$res = DBNastenka::query("SELECT u_login,u_jmeno,u_prijmeni,up_aktu,up_nadpis,up_text,up_lock" .
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
	
	public static function addNastenka($user, $nadpis, $text, $lock) {
		list($user, $nadpis, $text, $lock) = DBNastenka::escapeArray(array($user, $nadpis, $text, $lock));
		
		DBNastenka::query("INSERT INTO upozorneni (up_kdo,up_nadpis,up_text,up_lock) VALUES " .
			"((SELECT u_id FROM users WHERE u_login='$user'),'$nadpis','$text','$lock')");
		
		return true;
	}
	
	public static function editNastenka($id, $nadpis, $text, $lock) {
		list($id, $nadpis, $text, $lock) = DBNastenka::escapeArray(array($id, $nadpis, $text, $lock));
		
		DBNastenka::query("UPDATE upozorneni SET " .
			"up_nadpis='$nadpis',up_text='$text',up_lock='$lock' WHERE up_id='$id'");
		
		return true;
	}

	public static function removeNastenka($id) {
		list($id) = DBNastenka::escapeArray(array($id));
		
		DBNastenka::query("DELETE FROM upozorneni WHERE up_id='$id'");
		
		return true;
	}
}
?>