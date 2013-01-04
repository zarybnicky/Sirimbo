<?php
class DBRozpis extends Database {
public static function getRozpis() {
		$res = DBRozpis::query("SELECT u_jmeno,u_prijmeni,r_id,r_trener,r_kde,r_datum,r_visible,r_lock" .
			" FROM rozpis LEFT JOIN users ON r_trener=u_id ORDER BY r_datum");
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
	
	public static function rozpisSignOut($rid) {
		list($rid) = DBRozpis::escapeArray(array($rid));
		if(!DBRozpis::isRozpisFree($rid)) {
			$res = DBRozpis::query("UPDATE rozpis_item SET ri_partner='' WHERE ri_id='$rid'");
			return true;
		} else
			return false;
	}
	
	public static function getRozpisItem($rid) {
		list($rid) = DBRozpis::escapeArray(array($rid));
		
		$res = DBRozpis::query(
		"SELECT p_id,u_id,u_login,u_jmeno,u_prijmeni,ri_id,ri_id_rodic,ri_partner,
			ri_od,ri_do,ri_lock
		FROM rozpis_item
			LEFT JOIN pary ON ri_partner=p_id
			LEFT JOIN users ON p_id_partner=u_id
		WHERE ri_id_rodic='$rid'
		ORDER BY ri_od");
		return DBRozpis::getArray($res);
	}
	
	public static function getRozpisItemLesson($ri_id) {
		list($ri_id) = DBRozpis::escapeArray(array($ri_id));
		
		$res = DBRozpis::query("SELECT u_id,u_login,u_jmeno,u_prijmeni,r_trener,ri_id,ri_id_rodic,ri_partner," .
			"ri_od,ri_do,ri_lock FROM rozpis_item LEFT JOIN users ON ri_partner=u_id" .
			" LEFT JOIN rozpis ON ri_id_rodic=r_id WHERE ri_id='$ri_id'");
		return DBRozpis::getSingleRow($res);
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
		
		$res = DBRozpis::query("SELECT r_id,r_trener,u_jmeno,u_prijmeni,r_kde,r_datum,r_visible,r_lock" .
			" FROM rozpis LEFT JOIN users ON r_trener=u_id WHERE r_id='$id'");
		if(!$res) {
			return false;
		} else {
			return DBRozpis::getSingleRow($res);
		}
	}
	
	public static function getRozpisTrener($id) {
		list($id) = DBRozpis::escapeArray(array($id));
		
		$res = DBRozpis::query(
			"SELECT * FROM users
			WHERE u_id=(SELECT r_trener FROM rozpis WHERE r_id='$id')"
		);
		if(!$res) {
			return false;
		} else {
			return DBRozpis::getSingleRow($res);
		}
	}
	
	public static function addRozpis($trener, $kde, $datum, $visible, $lock) {
		list($trener, $kde, $datum, $visible, $lock) =
			DBRozpis::escapeArray(array($trener, $kde, $datum, $visible, $lock));
		
		DBRozpis::query("INSERT INTO rozpis (r_trener,r_kde,r_datum,r_visible,r_lock) VALUES " .
			"('$trener','$kde','$datum','$visible','$lock')");
		
		return true;
	}
	
	public static function editRozpis($id, $trener, $kde, $datum, $visible, $lock) {
		list($id, $trener, $kde, $datum, $visible, $lock) =
			DBRozpis::escapeArray(array($id, $trener, $kde, $datum, $visible, $lock));
		
		DBRozpis::query("UPDATE rozpis SET r_trener='$trener',r_kde='$kde',r_datum='$datum'," .
			"r_visible='$visible',r_lock='$lock' WHERE r_id='$id'");
		
		return true;
	}
	
	public static function removeRozpis($id) {
		list($id) = DBRozpis::escapeArray(array($id));
		
		DBRozpis::query("DELETE FROM rozpis WHERE r_id='$id'");
		DBRozpis::query("DELETE FROM rozpis_item WHERE ri_id_rodic='$id'");
		
		return true;
	}
	
	public static function isRozpisLocked($id) {
		list($id) = DBRozpis::escapeArray(array($id));
		
		$res = DBRozpis::query("SELECT r_lock FROM rozpis WHERE r_id='$id'");
		if(!$res) {
			return false;
		} else {
			$row = DBRozpis::getSingleRow($res);
			return (bool)$row["r_lock"];
		}
	}
	
	public static function isRozpisVisible($id) {
		list($id) = DBRozpis::escapeArray(array($id));
		
		$res = DBRozpis::query("SELECT r_visible FROM rozpis WHERE r_id='$id'");
		if(!$res) {
			return false;
		} else {
			$row = DBRozpis::getSingleRow($res);
			return (bool)$row["r_visible"];
		}
	}
	
	public static function addRozpisItem($parent_id, $user_id, $od, $do, $lock) {
		list($parent_id, $user_id, $od, $do, $lock) =
			DBRozpis::escapeArray(array($parent_id, $user_id, $od, $do, $lock));
		
		DBRozpis::query("INSERT INTO rozpis_item (ri_id_rodic,ri_partner,ri_od,ri_do,ri_lock)" .
			" VALUES ('$parent_id','$user_id','$od','$do','$lock')" .
			" ON DUPLICATE KEY UPDATE ri_partner='$user_id',ri_do='$do',ri_lock='$lock'");
		
		return true;
	}
	
	public static function editRozpisItem($id, $partner, $od, $do, $lock) {
		list($id, $partner, $od, $do, $lock) = DBRozpis::escapeArray(array($id, $partner, $od, $do, $lock));
		
		$res = DBNabidka::query("SELECT ri_id,ri_id_rodic FROM rozpis_item WHERE ri_od='$od' AND " .
			"ri_id_rodic=(SELECT ri_id_rodic FROM rozpis_item WHERE ri_id='$id')");
			//Finds conflicting rozpis
		
		if(!$res) {
			return false;
		} else {
			$row = DBNabidka::getSingleRow($res);
		}
		if($row['ri_id'] && $row['ri_id'] != $id) {//if there is a conflicting rozpis:
			DBRozpis::removeRozpisItem($id);
			DBRozpis::addRozpisItem($row['ri_id_rodic'], $partner, $od, $do, $lock);
		} else {
			DBRozpis::query("UPDATE rozpis_item SET ri_partner='$partner',ri_od='$od',ri_do='$do'," .
				"ri_lock='$lock' WHERE ri_id='$id'");
		}
		
		return true;
	}
	
	public static function removeRozpisItem($id) {
		list($id) = DBRozpis::escapeArray(array($id));
		
		DBRozpis::query("DELETE FROM rozpis_item WHERE ri_id='$id'");
		
		return true;
	}
}
?>