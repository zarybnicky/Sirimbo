<?php
class DBPlatby extends Database implements Pagable {
	public static function getInstance() { return new self(); }
	
	public static function getPage($offset, $count, $options = null) {
		if(!isset($options['type']) || $options['type'] == 'platby')
			$w = '';
		elseif($options['type'] == 'date' && isset($options['od']) && isset($options['do']))
			$w = "WHERE up_plati_do >= '{$options['od']}' AND up_plati_do <= '{$options['do']}'";
		elseif($options['type'] == 'user' && isset($options['id']))
			$w = "WHERE up_id_user='{$options['id']}'";
		else
			$w = '';
			
		$res = DBPlatby::query("SELECT * FROM users_platby
			LEFT JOIN users ON up_id_user=u_id
			LEFT JOIN users_skupiny ON u_skupina=us_id " . $w .
			" ORDER BY up_placeno DESC LIMIT $offset,$count");
		
		return DBPlatby::getArray($res);
	}
	public static function getCount($options = null) {
		if(!isset($options['type']) || $options['type'] == 'platby')
			$q = "SELECT COUNT(*) FROM users_platby";
		elseif($options['type'] == 'date' && !empty($options['od']) && !empty($options['do']))
			$q = "SELECT COUNT(*) FROM users_platby
			WHERE up_plati_do >= '{$options['od']}' AND up_plati_do <= '{$options['do']}'";
		elseif($options['type'] == 'user' && !empty($options['id']))
			$q = "SELECT COUNT(*) FROM users_platby
			WHERE up_id_user='{$options['id']}'";
		else
			$q = "SELECT COUNT(*) FROM users_platby";
		
		$res = DBPlatby::query($q);
		$res = DBPlatby::getSingleRow($res);
		return $res['COUNT(*)'];
	}
	
	public static function getPlatby($offset = null, $count = null) {
		list($offset, $count) = DBPlatby::escapeArray(array($offset, $count));
		
		$res = DBPlatby::query(
		"SELECT * FROM users_platby
			LEFT JOIN users ON up_id_user=u_id
			LEFT JOIN users_skupiny ON u_skupina=us_id
		ORDER BY up_placeno DESC" . 
		((!empty($offset) && !empty($count)) ? " LIMIT $offset,$count" : ''));
		return DBPlatby::getArray($res);
	}
	public static function getPlatbyByDate($od, $do, $offset = null, $count = null) {
		list($od, $do, $offset, $count) = DBPlatby::escapeArray(array($od, $do, $offset, $count));
		
		$res = DBPlatby::query(
		"SELECT * FROM users_platby
			LEFT JOIN users ON up_id_user=u_id
			LEFT JOIN users_skupiny ON u_skupina=us_id
		WHERE up_plati_do >= '$od' AND up_plati_do <= '$do'
		ORDER BY up_placeno DESC" .
		(($offset !== null && $count !== null) ? " LIMIT $offset,$count" : ''));
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
	public static function addPlatba($user, $obdobi, $castka, $placeno, $plati_do) {
		list($user, $obdobi, $castka, $placeno, $plati_do) =
			DBPlatby::escapeArray(array($user, $obdobi, $castka, $placeno, $plati_do));
		
		DBPlatby::query("INSERT INTO users_platby
		(up_id_user,up_obdobi,up_castka,up_placeno,up_plati_do)
		VALUES ('$user','$obdobi','$castka','$placeno','$plati_do')");
	}
	public static function editPlatba($id, $user, $obdobi, $castka, $placeno, $plati_do) {
		list($id, $user, $obdobi, $castka, $placeno, $plati_do) =
			DBPlatby::escapeArray(array($id, $user, $obdobi, $castka, $placeno, $plati_do));
		
		DBPlatby::query("UPDATE users_platby
		SET up_id_user='$user',up_obdobi='$obdobi',up_castka='$castka',up_placeno='$placeno',
			up_plati_do='$plati_do'
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