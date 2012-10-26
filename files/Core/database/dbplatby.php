<?php
class DBPlatby extends Database {
	public static function getPlatby() {
		$res = DBPlatby::query(
		"SELECT * FROM users_platby
			LEFT JOIN users ON up_id_user=u_id
			LEFT JOIN users_skupiny ON u_skupina=us_id
		ORDER BY up_placeno DESC");
		return DBPlatby::getArray($res);
	}
	public static function getPlatbyFromUser($id) {
		list($id) = DBPlatby::escapeArray(array($id));
		
		$res = DBPlatby::query(
		"SELECT * FROM users_platby
			LEFT JOIN users ON up_id_user=u_id
			LEFT JOIN users_skupiny ON u_skupina=us_id
		WHERE up_id_user='$id' ORDER BY up_placeno DESC");
		return DBPlatby::getArray($res);
	}
	public static function getSinglePlatba($id) {
		list($id) = DBPlatby::escapeArray(array($id));
		
		$res = DBPlatby::query(
		"SELECT * FROM users_platby
			LEFT JOIN users ON up_id_user=u_id
			LEFT JOIN users_skupiny ON u_skupina=us_id
		WHERE up_id='$id'");
		return DBPlatby::getSingleRow($res);
	}
	public static function addPlatba($user, $castka, $placeno, $plati_do) {
		list($user, $castka, $placeno, $plati_do) =
			DBPlatby::escapeArray(array($user, $castka, $placeno, $plati_do));
		
		DBPlatby::query("INSERT INTO users_platby
		(up_id_user,up_castka,up_placeno,up_plati_do)
		VALUES ('$user','$castka','$placeno','$plati_do')");
	}
	public static function editPlatba($id, $user, $castka, $placeno, $plati_do) {
		list($id, $user, $castka, $placeno, $plati_do) =
			DBPlatby::escapeArray(array($id, $user, $castka, $placeno, $plati_do));
		
		DBPlatby::query("UPDATE users_platby
		SET up_id_user='$user',up_castka='$castka',up_placeno='$placeno',up_plati_do='$plati_do'
		WHERE up_id='$id'");
		return true;
	}
	public static function removePlatba($id) {
		list($id) = DBPlatby::escapeArray(array($id));
		
		DBPlatby::query("DELETE FROM users_platby WHERE up_id='$id'");
		return true;
	}
	public static function removePlatbyFromUser($id) {
		list($id) = DBPlatby::escapeArray(array($id));
		
		DBPlatby::query("DELETE FROM users_platby WHERE up_id_user='$id'");
		return true;
	}
}
?>