<?php
class DBNastenka extends Database implements Pagable {
	public static function getInstance() { return new self(); }
	
	public static function getNastenka($offset = null, $count = null) {
		$res = DBNastenka::query(
		"SELECT up_id,up_kdo,u_jmeno,u_prijmeni,up_aktu,up_nadpis,up_text,up_barvy,up_lock
		FROM upozorneni
			LEFT JOIN users ON up_kdo=u_id
		ORDER BY up_aktu DESC" . (($offset !== null && $count !== null) ? " LIMIT $offset,$count" : ''));
		return DBNastenka::getArray($res);
	}
	
	public static function getPage($offset, $count, $options = '') {
		return DBNastenka::getNastenka($offset, $count);
	}
	public static function getCount($options = null) {
		return DBNastenka::getNastenkaCount();
	}
	
	public static function getNastenkaCount() {
		$res = DBNastenka::query(
		"SELECT COUNT(*) FROM upozorneni");
		$res = DBNastenka::getSingleRow($res);
		return $res['COUNT(*)'];
	}
	
	public static function getNastenkaSkupiny($id) {
		list($id) = DBNastenka::escapeArray(array($id));
		
		$res = DBNastenka::query("SELECT * FROM upozorneni_skupiny WHERE ups_id_rodic='$id'");
		return DBNastenka::getArray($res);
	}
	
	public static function addNastenkaSkupina($rodic, $skupina, $color, $popis) {
		list($rodic, $skupina, $color, $popis) =
			DBNastenka::escapeArray(array($rodic, $skupina, $color, $popis));
		
		DBNastenka::query("INSERT INTO upozorneni_skupiny (ups_id_rodic,ups_id_skupina,ups_color,ups_popis)
			VALUES ('$rodic','$skupina','$color','$popis')");
	}
	
	public static function removeNastenkaSkupina($id) {
		list($id) = DBNastenka::escapeArray(array($id));
		
		DBNastenka::query("DELETE FROM upozorneni_skupiny WHERE ups_id='$id'");
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
		
		$res = DBNastenka::query("SELECT up_kdo,u_jmeno,u_prijmeni,up_aktu,up_nadpis,up_text,up_barvy,up_lock" .
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
	
	public static function addNastenka($userid, $nadpis, $text, $lock) {
		list($userid, $nadpis, $text, $lock) =
			DBNastenka::escapeArray(array($userid, $nadpis, $text, $lock));
		
		DBNastenka::query("INSERT INTO upozorneni (up_kdo,up_nadpis,up_text,up_lock) VALUES " .
			"('$userid','$nadpis','$text','$lock')");
		return DBNastenka::getInsertId();
	}
	
	public static function editNastenka($id, $nadpis, $text, $lock) {
		list($id, $nadpis, $text, $lock) =
			DBNastenka::escapeArray(array($id, $nadpis, $text, $lock));
		
		DBNastenka::query("UPDATE upozorneni SET " .
			"up_nadpis='$nadpis',up_text='$text',up_lock='$lock' WHERE up_id='$id'");
		
		return true;
	}

	public static function removeNastenka($id) {
		list($id) = DBNastenka::escapeArray(array($id));
		
		DBNastenka::query("DELETE FROM upozorneni WHERE up_id='$id'");
		DBNastenka::query("DELETE FROM upozorneni_skupiny WHERE ups_id_rodic='$id'");
		
		return true;
	}
}
?>