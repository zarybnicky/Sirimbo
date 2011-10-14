<?php
class DBTaS extends Database {
	public static function getTaS() {
		$res = DBTaS::query("SELECT t_id,t_jmeno,t_kde,t_info,t_od,t_do,t_kapacita,t_dokumenty,t_lock FROM tas" .
			" ORDER BY t_od");
		return DBTaS::getArray($res);
	}
	
	public static function getSingleTaS($id) {
		list($id) = DBTaS::escapeArray(array($id));
		
		$res = DBTaS::query("SELECT t_id,t_jmeno,t_kde,t_info,t_od,t_do,t_kapacita,t_dokumenty,t_lock FROM tas" .
			" WHERE t_id='$id' ORDER BY t_od");
		if(!$res) {
			return false;
		} else {
			return DBTaS::getSingleRow($res);
		}
	}
	
	public static function getTaSItems($id) {
		list($id) = DBTaS::escapeArray(array($id));
		
		$res = DBTaS::query("SELECT ti_id,ti_id_rodic,ti_user,ti_jmeno,ti_prijmeni,ti_rok_narozeni" .
			" FROM tas_item WHERE ti_id_rodic='$id' ORDER BY ti_prijmeni");
		return DBTaS::getArray($res);
	}
	
	public static function addTaS($jmeno, $kde, $info, $od, $do, $kapacita, $dokumenty, $lock) {
		list($jmeno, $kde, $info, $od, $do, $kapacita, $dokumenty, $lock) =
			DBRozpis::escapeArray(array($jmeno, $kde, $info, $od, $do, $kapacita, $dokumenty, $lock));
		
		DBRozpis::query("INSERT INTO tas (t_jmeno,t_kde,t_info,t_od,t_do,t_kapacita,t_dokumenty,t_lock)" .
			" VALUES ('$jmeno','$kde','$info','$od','$do','$kapacita','$dokumenty','$lock')");
		
		return true;
	}
	
	public static function editTaS($id, $jmeno, $kde, $info, $od, $do, $kapacita, $dokumenty, $lock) {
		list($id, $jmeno, $kde, $info, $od, $do, $kapacita, $dokumenty, $lock) =
			DBRozpis::escapeArray(array($id, $jmeno, $kde, $info, $od, $do, $kapacita, $dokumenty, $lock));
		
		DBRozpis::query("UPDATE tas SET t_jmeno='$jmeno',t_kde='$kde',t_info='$info',t_od='$od',t_do='$do'," .
			"t_kapacita='$kapacita',t_dokumenty='$dokumenty',t_lock='$lock' WHERE t_id='$id'");
		
		return true;
	}
	
	public static function removeTaS($id) {
		list($id) = DBRozpis::escapeArray(array($id));
		
		DBRozpis::query("DELETE FROM tas WHERE t_id='$id'");
		DBRozpis::query("DELETE FROM tas_item WHERE ti_id_rodic='$id'");
		
		return true;
	}
	
	public static function signUp($user_id, $parent_id, $rok_narozeni) {
		list($user_id, $parent_id, $rok_narozeni) =
			DBTaS::escapeArray(array($user_id, $parent_id, $rok_narozeni));
		
		DBTaS::query("INSERT INTO tas_item (ti_id_rodic,ti_user,ti_jmeno,ti_prijmeni,ti_rok_narozeni)" .
			" VALUES ('$parent_id','$user_id',(SELECT u_jmeno FROM users WHERE u_id='$user_id')," .
			"(SELECT u_prijmeni FROM users WHERE u_id='$user_id'),'$rok_narozeni')");
		
		return true;
	}
	
	public static function signOut($user_id, $parent_id) {
		list($user_id, $parent_id) = DBTaS::escapeArray(array($user_id, $parent_id));
		
		DBTaS::query("DELETE FROM tas_item WHERE ti_user='$user_id' AND ti_id_rodic='$parent_id'");
		
		return true;
	}
	
	public static function getTaSName($id) {
		list($id) = DBRozpis::escapeArray(array($id));
		
		$res = DBRozpis::query("SELECT t_jmeno FROM tas WHERE t_id='$id'");
		if(!$res) {
			return false;
		} else {
			$row = DBRozpis::getSingleRow($res);
			
			return $row["t_jmeno"];
		}
	}
	
	public static function addTaSItem($p_id, $u_id, $jmeno, $prijmeni, $rok) {
		list($p_id, $u_id, $jmeno, $prijmeni, $rok) =
			DBRozpis::escapeArray(array($p_id, $u_id, $jmeno, $prijmeni, $rok));
		
		DBRozpis::query("INSERT INTO tas_item (ti_id_rodic,ti_user,ti_jmeno,ti_prijmeni,ti_rok_narozeni)" .
			" VALUES ('$p_id','$u_id','$jmeno','$prijmeni','$rok')");
		
		return true;
	}
	
	public static function editTaSItem($id, $u_id, $jmeno, $prijmeni, $rok) {
		list($id, $u_id, $jmeno, $prijmeni, $rok) =
			DBRozpis::escapeArray(array($id, $u_id, $jmeno, $prijmeni, $rok));
	
		DBRozpis::query("UPDATE tas_item SET ti_user='$u_id',ti_jmeno='$jmeno',ti_prijmeni='$prijmeni' WHERE" .
			" ti_id='$id'");
				
		return true;
	}
	
	public static function removeTaSItem($id) {
		list($id) = DBRozpis::escapeArray(array($id));
		
		DBRozpis::query("DELETE FROM tas_item WHERE ti_id='$id'");
		
		return true;
	}
	
	public static function isUserSignedUp($u_id) {
		list($u_id) = DBRozpis::escapeArray(array($u_id));
		
		$res = DBRozpis::query("SELECT ti_id FROM tas_item WHERE ti_user='$u_id'");
		if(!$res) {
			return false;
		} else {
			$row = DBRozpis::getSingleRow($res);
			return !(bool)$row["ti_id"];
		}
	}
}
?>