<?php
class DBAkce extends Database {
	public static function getAkce() {
		$res = DBAkce::query("SELECT a_id,a_jmeno,a_kde,a_info,a_od,a_do,a_kapacita,a_dokumenty,a_lock FROM akce" .
			" ORDER BY a_od");
		return DBAkce::getArray($res);
	}
	
	public static function getSingleAkce($id) {
		list($id) = DBAkce::escapeArray(array($id));
		
		$res = DBAkce::query("SELECT a_id,a_jmeno,a_kde,a_info,a_od,a_do,a_kapacita,a_dokumenty,a_lock FROM akce" .
			" WHERE a_id='$id' ORDER BY a_od");
		if(!$res) {
			return false;
		} else {
			return DBAkce::getSingleRow($res);
		}
	}
	
	public static function getAkceItems($id) {
		list($id) = DBAkce::escapeArray(array($id));
		
		$res = DBAkce::query("SELECT ai_id,ai_id_rodic,ai_user,ai_jmeno,ai_prijmeni,ai_rok_narozeni" .
			" FROM akce_item WHERE ai_id_rodic='$id' ORDER BY ai_prijmeni");
		return DBAkce::getArray($res);
	}
	
	public static function addAkce($jmeno, $kde, $info, $od, $do, $kapacita, $dokumenty, $lock) {
		list($jmeno, $kde, $info, $od, $do, $kapacita, $dokumenty, $lock) =
			DBRozpis::escapeArray(array($jmeno, $kde, $info, $od, $do, $kapacita, $dokumenty, $lock));
		
		DBRozpis::query("INSERT INTO akce (a_jmeno,a_kde,a_info,a_od,a_do,a_kapacita,a_dokumenty,a_lock)" .
			" VALUES ('$jmeno','$kde','$info','$od','$do','$kapacita','$dokumenty','$lock')");
		
		return true;
	}
	
	public static function editAkce($id, $jmeno, $kde, $info, $od, $do, $kapacita, $dokumenty, $lock) {
		list($id, $jmeno, $kde, $info, $od, $do, $kapacita, $dokumenty, $lock) =
			DBRozpis::escapeArray(array($id, $jmeno, $kde, $info, $od, $do, $kapacita, $dokumenty, $lock));
		
		DBRozpis::query("UPDATE akce SET a_jmeno='$jmeno',a_kde='$kde',a_info='$info',a_od='$od',a_do='$do'," .
			"a_kapacita='$kapacita',a_dokumenty='$dokumenty',a_lock='$lock' WHERE a_id='$id'");
		
		return true;
	}
	
	public static function removeAkce($id) {
		list($id) = DBRozpis::escapeArray(array($id));
		
		DBRozpis::query("DELETE FROM akce WHERE a_id='$id'");
		DBRozpis::query("DELETE FROM akce_item WHERE ai_id_rodic='$id'");
		
		return true;
	}
	
	public static function signUp($user_id, $parena_id, $rok_narozeni) {
		list($user_id, $parena_id, $rok_narozeni) =
			DBAkce::escapeArray(array($user_id, $parena_id, $rok_narozeni));
		
		DBAkce::query("INSERT INTO akce_item (ai_id_rodic,ai_user,ai_jmeno,ai_prijmeni,ai_rok_narozeni)" .
			" VALUES ('$parena_id','$user_id',(SELECT u_jmeno FROM users WHERE u_id='$user_id')," .
			"(SELECT u_prijmeni FROM users WHERE u_id='$user_id'),'$rok_narozeni')");
		
		return true;
	}
	
	public static function signOut($user_id, $parena_id) {
		list($user_id, $parena_id) = DBAkce::escapeArray(array($user_id, $parena_id));
		
		DBAkce::query("DELETE FROM akce_item WHERE ai_user='$user_id' AND ai_id_rodic='$parena_id'");
		
		return true;
	}
	
	public static function getAkceName($id) {
		list($id) = DBRozpis::escapeArray(array($id));
		
		$res = DBRozpis::query("SELECT a_jmeno FROM akce WHERE a_id='$id'");
		if(!$res) {
			return false;
		} else {
			$row = DBRozpis::getSingleRow($res);
			
			return $row["a_jmeno"];
		}
	}
	
	public static function addAkceItem($p_id, $u_id, $jmeno, $prijmeni, $rok) {
		list($p_id, $u_id, $jmeno, $prijmeni, $rok) =
			DBRozpis::escapeArray(array($p_id, $u_id, $jmeno, $prijmeni, $rok));
		
		DBRozpis::query("INSERT INTO akce_item (ai_id_rodic,ai_user,ai_jmeno,ai_prijmeni,ai_rok_narozeni)" .
			" VALUES ('$p_id','$u_id','$jmeno','$prijmeni','$rok')");
		
		return true;
	}
	
	public static function editAkceItem($id, $u_id, $jmeno, $prijmeni, $rok) {
		list($id, $u_id, $jmeno, $prijmeni, $rok) =
			DBRozpis::escapeArray(array($id, $u_id, $jmeno, $prijmeni, $rok));
	
		DBRozpis::query("UPDATE akce_item SET ai_user='$u_id',ai_jmeno='$jmeno',ai_prijmeni='$prijmeni' WHERE" .
			" ai_id='$id'");
				
		return true;
	}
	
	public static function removeAkceItem($id) {
		list($id) = DBRozpis::escapeArray(array($id));
		
		DBRozpis::query("DELETE FROM akce_item WHERE ai_id='$id'");
		
		return true;
	}
	
	public static function isUserSignedUp($u_id) {
		list($u_id) = DBRozpis::escapeArray(array($u_id));
		
		$res = DBRozpis::query("SELECT ai_id FROM akce_item WHERE ai_user='$u_id'");
		if(!$res) {
			return false;
		} else {
			$row = DBRozpis::getSingleRow($res);
			return !(bool)$row["ai_id"];
		}
	}
}
?>