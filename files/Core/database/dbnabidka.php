<?php
class DBNabidka extends Database {
public static function getNabidka() {
		$res = DBNabidka::query("SELECT u_jmeno,u_prijmeni,n_id,n_trener,n_pocet_hod,n_od,n_do,n_lock" .
			" FROM nabidka LEFT JOIN users ON n_trener=u_id");
		return DBNabidka::getArray($res);
	}
	
	public static function getNabidkaMaxLessons($id) {
		list($id) = DBNabidka::escapeArray(array($id));
		
		$res = DBNabidka::query("SELECT n_pocet_hod FROM nabidka WHERE n_id='$id'");
		if(!$res) {
			return false;
		} else {
			$row = DBNabidka::getSingleRow($res);
			return $row["n_pocet_hod"];
		}
	}
	
	public static function getSingleNabidka($id) {
		list($id) = DBNabidka::escapeArray(array($id));
		
		$res = DBNabidka::query("SELECT n_id,n_trener,u_jmeno,u_prijmeni,n_pocet_hod,n_od,n_do,n_lock" .
			" FROM nabidka LEFT JOIN users ON n_trener=u_id WHERE n_id='$id'");
		if(!$res) {
			return false;
		} else {
			return DBNabidka::getSingleRow($res);
		}
	}
	
	public static function getNabidkaTrenerID($id) {
		list($id) = DBNabidka::escapeArray(array($id));
		
		$res = DBNabidka::query("SELECT n_trener FROM nabidka WHERE n_id='$id'");
		if(!$res) {
			return false;
		} else {
			$row = DBNabidka::getSingleRow($res);
			return $row["n_trener"];
		}
	}
	
	public static function addNabidka($trener, $pocet_hod, $od, $do, $lock) {
		list($trener, $pocet_hod, $od, $do, $lock) =
			DBNabidka::escapeArray(array($trener, $pocet_hod, $od, $do, $lock));
		
		DBNabidka::query("INSERT INTO nabidka (n_trener,n_pocet_hod,n_od,n_do,n_lock) VALUES " .
			"('$trener','$pocet_hod','$od','$do','$lock')");
		
		return true;
	}
	
	public static function editNabidka($id, $trener, $pocet_hod, $od, $do, $lock) {
		list($id, $trener, $pocet_hod, $od, $do, $lock) =
			DBNabidka::escapeArray(array($id, $trener, $pocet_hod, $od, $do, $lock));
		
		DBNabidka::query("UPDATE nabidka SET n_trener='$trener',n_pocet_hod='$pocet_hod',n_od='$od'," .
			"n_do='$do',n_lock='$lock' WHERE n_id='$id'");
		
		return true;
	}
	
	public static function removeNabidka($id) {
		list($id) = DBNabidka::escapeArray(array($id));
		
		DBNabidka::query("DELETE FROM nabidka WHERE n_id='$id'");
		DBNabidka::query("DELETE FROM nabidka_item WHERE ni_id_rodic='$id'");
		
		return true;
		
	}
	
	public static function getNabidkaItem($parent_id) {
		list($parent_id) = DBNabidka::escapeArray(array($parent_id));
		
		$res = DBNabidka::query("SELECT u_id,u_login,u_jmeno,u_prijmeni,ni_id,ni_id_rodic,ni_partner," .
			"ni_pocet_hod,ni_lock FROM nabidka_item LEFT JOIN users ON ni_partner=u_id WHERE " .
			"ni_id_rodic='$parent_id'");
		return DBNabidka::getArray($res);
	}
	
	public static function getNabidkaItemLessons($parent_id) {
		list($parent_id) = DBNabidka::escapeArray(array($parent_id));
		
		$res = DBNabidka::query("SELECT SUM(ni_pocet_hod) FROM nabidka_item WHERE ni_id_rodic='$parent_id'");
		if(!$res) {
			return false;
		} else {
			$row = DBNabidka::getSingleRow($res);
			return $row["SUM(ni_pocet_hod)"];
		}
	}
	
	public static function hasNabidkaLessons($parent_id, $u_id) {
		list($parent_id, $u_id) = DBNabidka::escapeArray(array($parent_id, $u_id));
		
		$res = DBNabidka::query("SELECT ni_pocet_hod FROM nabidka_item WHERE ni_id_rodic='$parent_id' AND " .
			"ni_partner='$u_id'");
		if(!$res) {
			return false;
		} else {
			$row = DBNabidka::getSingleRow($res);
			return (bool)$row["ni_pocet_hod"];
		}
	}
	
	public static function addNabidkaItemLessons($login, $parent_id, $pocet_hod) {
		list($login, $parent_id, $pocet_hod) = DBNabidka::escapeArray(array($login, $parent_id, $pocet_hod));
		
		DBNabidka::query("INSERT INTO nabidka_item (ni_partner,ni_id_rodic,ni_pocet_hod)" .
			" VALUES ((SELECT u_id FROM users WHERE u_login='$login'),'$parent_id','$pocet_hod')" .
			" ON DUPLICATE KEY UPDATE ni_pocet_hod=ni_pocet_hod+'$pocet_hod'");
		
		return true;
	}
	
	public static function removeNabidkaItem($parent_id, $u_id) {
		list($parent_id, $u_id) = DBNabidka::escapeArray(array($parent_id, $u_id));
		
		DBNabidka::query("DELETE FROM nabidka_item WHERE ni_id_rodic='$parent_id' AND " .
			"ni_partner='$u_id'");
		
		return true;
	}
}
?>