<?php
class DBRozpis extends Database {
public static function getRozpis() {
		$res = DBRozpis::query("SELECT u_jmeno,u_prijmeni,r_id,r_trener,r_kde,r_datum,r_od,r_do,r_lock" .
			" FROM rozpis LEFT JOIN users ON r_trener=u_id");
		return DBRozpis::getArray($res);
	}
	
	public static function rozpisSignUp($rid, $uid) {
		list($rid, $uid) = DBRozpis::escapeArray(array($rid, $uid));
		if(DBRozpis::isRozpisFree($rid)) {
			$res = DBRozpis::query("UPDATE rozpis_item SET ri_partner='$uid' WHERE ri_id='$rid'");
			return true;
		} else
			return false;
	}
	
	public static function rozpisSignOut($rid, $uid) {
		list($rid, $uid) = DBRozpis::escapeArray(array($rid, $uid));
		if(!DBRozpis::isRozpisFree($rid)) {
			$res = DBRozpis::query("UPDATE rozpis_item SET ri_partner='' WHERE ri_id='$rid'");
			return true;
		} else
			return false;
	}
	
	public static function getRozpisItem($rid) {
		list($rid) = DBRozpis::escapeArray(array($rid));
		
		$res = DBRozpis::query("SELECT u_id,u_login,u_jmeno,u_prijmeni,ri_id,ri_id_rodic,ri_partner," .
			"ri_od,ri_do,ri_lock FROM rozpis_item LEFT JOIN users ON ri_partner=u_id WHERE " .
			"ri_id_rodic='$rid'");
		return DBRozpis::getArray($res);
	}
	
	public static function getRozpisItemLesson($ri_id) {
		list($ri_id) = DBRozpis::escapeArray(array($ri_id));
		
		$res = DBRozpis::query("SELECT u_id,u_login,u_jmeno,u_prijmeni,r_trener,ri_id,ri_id_rodic,ri_partner," .
			"ri_od,ri_do,ri_lock FROM rozpis_item LEFT JOIN users ON ri_partner=u_id LEFT JOIN rozpis ON ri_id_rodic=r_id WHERE " .
			"ri_id='$ri_id'");
		return DBRozpis::getArray($res);
	}

	public static function isRozpisFree($rid) {
	list($rid) = DBRozpis::escapeArray(array($rid));
		
		$res = DBRozpis::query("SELECT ri_partner FROM rozpis_item WHERE ri_id='$rid'");
		if(!$res) {
			return false;
		} else {
			$row = DBRozpis::getSingleRow($res);
			return !(bool)$row["ri_partner"];
		}
	}
	
	public static function getSingleRozpis($id) {
		list($id) = DBRozpis::escapeArray(array($id));
		
		$res = DBRozpis::query("SELECT r_id,r_trener,u_jmeno,u_prijmeni,r_kde,r_datum,r_od,r_do,r_lock" .
			" FROM rozpis LEFT JOIN users ON r_trener=u_id WHERE r_id='$id'");
		if(!$res) {
			return false;
		} else {
			return DBRozpis::getSingleRow($res);
		}
	}
	
	public static function getRozpisTrenerID($id) {
		list($id) = DBRozpis::escapeArray(array($id));
		
		$res = DBRozpis::query("SELECT r_trener FROM rozpis WHERE r_id='$id'");
		if(!$res) {
			return false;
		} else {
			$row = DBRozpis::getSingleRow($res);
			return $row["r_trener"];
		}
	}
	
	public static function addRozpis($trener, $kde, $datum, $od, $do, $lock) {
		list($trener, $kde, $datum, $od, $do, $lock) =
			DBRozpis::escapeArray(array($trener, $kde, $datum, $od, $do, $lock));
		
		DBRozpis::query("INSERT INTO rozpis (r_trener,r_kde,r_datum,r_od,r_do,r_lock) VALUES " .
			"('$trener','$kde','$datum','$od','$do','$lock')");
		
		return true;
	}
	
	public static function editRozpis($id, $trener, $kde, $datum, $od, $do, $lock) {
		list($id, $trener, $kde, $datum, $od, $do, $lock) =
			DBRozpis::escapeArray(array($id, $trener, $kde, $datum, $od, $do, $lock));
		
		DBRozpis::query("UPDATE rozpis SET r_trener='$trener',r_kde='$kde',r_datum='$datum'," .
			"r_od='$od',r_do='$do',r_lock='$lock' WHERE r_id='$id'");
		
		return true;
	}
	
	public static function removeRozpis($id) {
		list($id) = DBRozpis::escapeArray(array($id));
		
		DBRozpis::query("DELETE FROM rozpis WHERE r_id='$id'");
		DBRozpis::query("DELETE FROM rozpis_item WHERE ri_id_rodic='$id'");
		
		return true;
	}
}
?>