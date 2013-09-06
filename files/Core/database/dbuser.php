<?php
class DBUser extends Database implements Pagable {
	public static function getInstance() { return new self(); }

	public static function getPage($offset, $count, $options = null) {
		if(!isset($options['filter'])) 
			$options['filter'] = 'all';
		if(!isset($options['sort']))
			$options['sort'] = 'prijmeni';
		
		$q = "SELECT users.*,p1.*,users_skupiny.* FROM users
			LEFT JOIN users_platby p1 ON up_id_user=u_id
			LEFT JOIN users_skupiny ON users.u_skupina=users_skupiny.us_id
		WHERE (p1.up_id IS NULL OR
			(p1.up_plati_do >= CURDATE() AND p1.up_plati_od <= CURDATE()) OR
			(p1.up_plati_do = (SELECT MAX(p2.up_plati_do) FROM users_platby p2
				WHERE p2.up_id_user=u_id)))";
		switch($options['filter']) {
			case 'unconfirmed': $q .= " AND u_confirmed='0' AND u_ban='0'"; break;
			case 'ban': $q .= " AND u_ban='1'"; break;
			case 'dancer': $q .= " AND u_confirmed='1' AND u_ban='0' AND u_system='0' AND u_dancer='1'"; break;
			case 'system': $q .= " AND u_confirmed='1' AND u_ban='0' AND u_system='1'"; break;
			case 'all':
			default:
				if(is_numeric($options['filter']))
					$q .= " AND u_confirmed='1' AND u_ban='0' AND u_system='0' AND u_group='{$options['filter']}'";
				else
					$q .= " AND u_confirmed='1' AND u_ban='0' AND u_system='0'";
		}
		switch($options['sort']) {
			case 'var-symbol':	$q .= ' ORDER BY u_id'; break;
			case 'narozeni':	$q .= ' ORDER BY u_narozeni'; break;
			case 'prijmeni':
			default:			$q .= ' ORDER BY u_prijmeni'; break;
		}
		$q .= " LIMIT $offset,$count";
		$res = DBUser::query($q);
		return DBUser::getArray($res);
	}
	public static function getCount($options = null) {
		if(!isset($options['filter']))
			$options['filter'] = 'all';
		
		$q = "SELECT COUNT(*) FROM users WHERE 1=1";
		switch($options['filter']) {
			case 'unconfirmed': $q .= " AND u_confirmed='0' AND u_ban='0'"; break;
			case 'ban': $q .= " AND u_ban='1'"; break;
			case 'dancer': $q .= " AND u_confirmed='1' AND u_ban='0' AND u_system='0' AND u_dancer='1'"; break;
			case 'system': $q .= " AND u_confirmed='1' AND u_ban='0' AND u_system='1'"; break;
			case 'all':
			default:
				if(is_numeric($options['filter']))
					$q .= " AND u_confirmed='1' AND u_ban='0' AND u_system='0' AND u_group='{$options['filter']}'";
				else
					$q .= " AND u_confirmed='1' AND u_ban='0' AND u_system='0'";
		}
		$q .= ' ORDER BY u_prijmeni';
		
		$res = DBUser::query($q);
		$res = DBUser::getSingleRow($res);
		return $res['COUNT(*)'];
	}
	
	public static function checkUser($login, $pass) {
		list($login, $pass) = DBUser::escapeArray(array($login, $pass));
		
		$res = DBUser::query("SELECT * FROM users WHERE 
			LOWER(u_login)=LOWER('$login') AND u_pass='$pass'");
		if(!$res) {
			return false;
		} else {
			$row = DBUser::getSingleRow($res);
			return $row["u_id"];
		}
	}
	
	public static function getUserGroup($id) {
		list($id) = DBUser::escapeArray(array($id));
		
		$res = DBUser::query("SELECT u_group FROM users WHERE " .
			"u_id='$id'");
		if(!$res) {
			return false;
		} else {
			$row = DBUser::getSingleRow($res);
			return $row["u_group"];
		}
	}
	
	public static function getUserID($login) {
		list($login) = DBUser::escapeArray(array($login));
		
		$res = DBUser::query("SELECT u_id FROM users WHERE " .
			"u_login='$login'");
		if(!$res) {
			return false;
		} else {
			$row = DBUser::getSingleRow($res);
			return $row["u_id"];
		}
	}
	
	public static function getUserDataByNameEmail($login, $email) {
		list($login, $email) = DBUser::escapeArray(array($login, $email));
		
		$res = DBUser::query(
		"SELECT *
		FROM users
		WHERE LOWER(u_login)=LOWER('$login') AND u_email='$email'");
		if(!$res) {
			return false;
		} else {
			return DBUser::getSingleRow($res);
		}
	}

	public static function getUserData($id) {
		list($id) = DBUser::escapeArray(array($id));
		
		$res = DBUser::query(
		"SELECT users.*,p1.*,users_skupiny.*,permissions.* FROM users
			LEFT JOIN users_platby p1 ON up_id_user=u_id
			LEFT JOIN users_skupiny ON users.u_skupina=users_skupiny.us_id
			LEFT JOIN permissions ON u_group=pe_id
		WHERE u_id='$id' AND (p1.up_id IS NULL OR
			(p1.up_plati_do >= CURDATE() AND p1.up_plati_od <= CURDATE()) OR
			(p1.up_placeno = (SELECT MAX(p2.up_placeno) FROM users_platby p2
				WHERE p2.up_id_user=u_id) AND p1.up_plati_do < CURDATE()))");
		if(!$res) {
			return false;
		} else {
			return DBUser::getSingleRow($res);
		}
	}
	
	public static function getUserByFullName($jmeno, $prijmeni) {
		list($jmeno, $prijmeni) = DBUser::escapeArray(array($jmeno, $prijmeni));
		
		$res = DBUser::query(
		"SELECT *
		FROM users
		WHERE u_jmeno='$jmeno' AND u_prijmeni='$prijmeni'");
		if(!$res) {
			return false;
		} else {
			return DBUser::getSingleRow($res);
		}
	}
	public static function addTemporaryUser($login, $jmeno, $prijmeni, $narozeni) {
		list($login, $jmeno, $prijmeni, $narozeni) =
			DBUser::escapeArray(array($login, $jmeno, $prijmeni, $narozeni));
		
		DBUser::query(
		"INSERT INTO users
			(u_login,u_pass,u_jmeno,u_prijmeni,u_narozeni,u_confirmed,u_temporary,u_system,u_group)
		VALUES ('$login','','$jmeno','$prijmeni','$narozeni','1','1','1','0')");
		$user_id = mysql_insert_id();
		
		DBUser::query("INSERT INTO pary (p_id_partner, p_archiv) VALUES ('" . $user_id . "','0')");
		$par_id = mysql_insert_id();
		
		return array($user_id, $par_id);
	}
	
	public static function isUserLocked($id) {
		list($id) = DBUser::escapeArray(array($id));
		
		$res = DBUser::query("SELECT u_lock FROM users WHERE u_id='$id'");
		if(!$res) {
			return false;
		} else {
			$row = DBUser::getSingleRow($res);
			return (bool) $row["u_lock"];
		}
	}
	
	public static function isUserConfirmed($id) {
		list($id) = DBUser::escapeArray(array($id));
		
		$res = DBUser::query("SELECT u_confirmed FROM users WHERE u_id='$id'");
		if(!$res) {
			return false;
		} else {
			$row = DBUser::getSingleRow($res);
			return (bool) $row["u_confirmed"];
		}
	}
	
	public static function confirmUser($id, $group, $skupina = '1', $dancer = 0) {
		list($id, $group, $skupina, $dancer) =
			DBUser::escapeArray(array($id, $group, $skupina, $dancer));
		
		DBUser::query("UPDATE users SET u_confirmed='1',u_group='$group',
			u_skupina='$skupina',u_dancer='$dancer',u_system='0' WHERE u_id='$id'");
	}
	
	public static function setPassword($id, $passwd) {
		list($id, $passwd) = DBUser::escapeArray(array($id, $passwd));
		
		DBUser::query("UPDATE users SET u_pass='$passwd' WHERE u_id='$id'");
		return true;
	}
	
	public static function setUserData($id, $jmeno, $prijmeni, $pohlavi, $email, $telefon,
			$narozeni, $poznamky, $group, $skupina, $dancer, $lock, $ban, $system) {
		list($id, $jmeno, $prijmeni, $pohlavi, $email, $telefon, $narozeni, $poznamky, $group,
			$skupina, $dancer, $lock, $ban, $system) = DBUser::escapeArray(array($id, $jmeno,
			$prijmeni, $pohlavi, $email, $telefon, $narozeni, $poznamky, $group, $skupina,
			$dancer, $lock, $ban, $system));
			
		DBUser::query("UPDATE users SET " .
			"u_jmeno='$jmeno',u_prijmeni='$prijmeni',u_pohlavi='$pohlavi',u_email='$email'," .
			"u_telefon='$telefon',u_narozeni='$narozeni',u_poznamky='$poznamky',u_group='$group'," .
			"u_skupina='$skupina',u_dancer='$dancer',u_lock='$lock',u_ban='$ban',u_system='$system'" .
			" WHERE u_id='$id'");
		return true;	
	}
	
	public static function addUser($login, $pass, $jmeno, $prijmeni, $pohlavi, $email, $telefon,
			$narozeni, $poznamky, $group, $skupina, $dancer, $lock, $ban, $confirmed, $system) {
		
		list($login, $pass, $jmeno, $prijmeni, $pohlavi, $email, $telefon, $narozeni, $poznamky,
			$group, $skupina, $dancer, $lock, $ban, $confirmed, $system) =
				DBUser::escapeArray(array($login, $pass, $jmeno, $prijmeni, $pohlavi, $email,
				$telefon, $narozeni, $poznamky, $group, $skupina, $dancer, $lock, $ban,
				$confirmed, $system));
		
		DBUser::query("INSERT INTO users " .
			"(u_login,u_pass,u_jmeno,u_prijmeni,u_pohlavi,u_email,u_telefon,u_narozeni," .
			"u_poznamky,u_group,u_skupina,u_dancer,u_lock,u_ban,u_confirmed,u_system) VALUES " .
			"('$login','$pass','$jmeno','$prijmeni','$pohlavi','$email','$telefon','$narozeni'," .
			"'$poznamky','$group','$skupina','$dancer','$lock','$ban','$confirmed','$system')");
		DBUser::query("INSERT INTO pary (p_id_partner) VALUES " .
			"((SELECT u_id FROM users WHERE u_login='$login'))");
		return true;
	}
	
	public static function removeUser($id) {
		list($id) = DBUser::escapeArray(array($id));
		
		DBUser::query("DELETE FROM users WHERE u_id='$id'");
		DBUser::query("DELETE FROM rozpis WHERE r_trener='$id'");
		DBUser::query("DELETE FROM rozpis_item WHERE ri_partner='$id'");
		DBUser::query("DELETE FROM nabidka WHERE n_trener='$id'");
		DBUser::query("DELETE FROM nabidka_item WHERE ni_partner='$id'");
		DBUser::query("DELETE FROM akce_item WHERE ai_user='$id'");
		
		DBPary::noPartner($id);
		DBUser::query("DELETE FROM pary WHERE p_id_partner='$id' AND p_archiv='0'");
		
		return true;
	}
	
	public static function getUsers($group = NULL) {
		$res = DBUser::query(
		"SELECT users.*,p1.*,users_skupiny.* FROM users
			LEFT JOIN users_platby p1 ON up_id_user=u_id
			LEFT JOIN users_skupiny ON users.u_skupina=users_skupiny.us_id
		WHERE (p1.up_id IS NULL OR
			(p1.up_plati_do >= CURDATE() AND p1.up_plati_od <= CURDATE()) OR
			(p1.up_placeno = (SELECT MAX(p2.up_placeno) FROM users_platby p2
				WHERE p2.up_id_user=u_id) AND p1.up_plati_do < CURDATE()))" .
		(($group == NULL || $group == L_ALL) ? '' : " AND u_group='$group'") .
		" ORDER BY u_prijmeni");
		
		return DBUser::getArray($res);
	}
	
	public static function getUsersByPohlavi($pohlavi) {
		list($pohlavi) = DBUser::escapeArray(array($pohlavi));
		
		$res = DBUser::query(
		"SELECT * FROM users
			LEFT JOIN users_skupiny ON users.u_skupina=users_skupiny.us_id
		WHERE u_pohlavi='$pohlavi' ORDER BY u_prijmeni");
		return DBUser::getArray($res);
	}
	
	public static function getUsersBySkupina($skupina, $noBanned = true) {
		list($skupina, $noBanned) = DBUser::escapeArray(array($skupina, $noBanned));
		
		$res = DBUser::query(
			"SELECT users.*,p1.*,users_skupiny.* FROM users
				LEFT JOIN users_platby p1 ON up_id_user=u_id
				LEFT JOIN users_skupiny ON users.u_skupina=users_skupiny.us_id
			WHERE u_skupina='$skupina' AND u_system='0'" .
					($noBanned ? " AND u_ban='0'" : '') . " AND (
				(p1.up_plati_od <= CURDATE() AND p1.up_plati_do >= CURDATE()) OR
				(
					((p1.up_placeno = (SELECT MAX(p2.up_placeno) FROM users_platby p2
							WHERE p2.up_id_user=u_id) AND
						p1.up_plati_do = (SELECT MAX(p2.up_plati_do) FROM users_platby p2
							WHERE p2.up_id_user=u_id AND p2.up_plati_do < CURDATE())) OR
					p1.up_id IS NULL)
					AND NOT EXISTS (
						SELECT * FROM users_platby p2
						WHERE p2.up_id_user=u_id AND
							p2.up_plati_od <= CURDATE() AND p2.up_plati_do >= CURDATE())
				)
			)
			ORDER BY u_prijmeni");
		return DBUser::getArray($res);
	}
	
	public static function getUsersByPermission($module, $permission) {
		list($module, $permission) = DBUser::escapeArray(array($module, $permission));
		$res = DBUser::query(
		"SELECT users.*,users_skupiny.* FROM users
			LEFT JOIN permissions ON u_group=pe_id
			LEFT JOIN users_skupiny ON u_skupina=us_id
		WHERE pe_$module >= '$permission'
		ORDER BY u_prijmeni");
		return DBUser::getArray($res);
	}
	
	public static function getNewUsers() {
		$res = DBUser::query(
		"SELECT * FROM users
			LEFT JOIN users_skupiny ON users.u_skupina=users_skupiny.us_id
		WHERE u_confirmed='0' ORDER BY u_prijmeni");
		return DBUser::getArray($res);
	}
	
	public static function getDuplicateUsers() {
		$res = DBUser::query(
		"SELECT u1.*,users_skupiny.* FROM users u1
			LEFT JOIN users_skupiny ON u1.u_skupina=users_skupiny.us_id
		WHERE EXISTS (SELECT * FROM users u2 WHERE
			((u1.u_jmeno=u2.u_jmeno AND u1.u_prijmeni=u2.u_prijmeni) OR
			u1.u_email=u2.u_email OR u1.u_telefon=u2.u_telefon) AND u1.u_id!=u2.u_id)
		ORDER BY u_email, u_telefon, u_prijmeni");
		return DBUser::getArray($res);
	}
	
	public static function getBannedUsers() {
		$res = DBUser::query(
		"SELECT u1.*,users_skupiny.* FROM users u1
			LEFT JOIN users_skupiny ON u1.u_skupina=users_skupiny.us_id
		WHERE u_ban='1' ORDER BY u_prijmeni");
		return DBUser::getArray($res);
	}
	
	public static function getUnconfirmedUsers() {
		$res = DBUser::query(
		"SELECT u1.*,users_skupiny.* FROM users u1
			LEFT JOIN users_skupiny ON u1.u_skupina=users_skupiny.us_id
		WHERE u_banconfirmed='0' ORDER BY u_prijmeni");
		return DBUser::getArray($res);
	}
	
	public static function getActiveUsers($group = L_ALL) {
		$res = DBUser::query(
		"SELECT users.*,p1.*,users_skupiny.* FROM users
			LEFT JOIN users_platby p1 ON up_id_user=u_id
			LEFT JOIN users_skupiny ON users.u_skupina=users_skupiny.us_id
		WHERE u_system='0' AND u_confirmed='1' AND u_ban='0' " .
			($group > L_ALL ? "AND u_group='$group' " : '') .
			"AND (p1.up_id IS NULL OR
			(p1.up_plati_do >= CURDATE() AND p1.up_plati_od <= CURDATE()) OR
			(p1.up_placeno = (SELECT MAX(p2.up_placeno) FROM users_platby p2
				WHERE p2.up_id_user=u_id) AND p1.up_plati_do < CURDATE()))
			ORDER BY u_prijmeni ");
		return DBUser::getArray($res);
	}
	
	public static function getActiveDancers($group = L_ALL) {
		$res = DBUser::query(
		"SELECT users.*,p1.*,users_skupiny.* FROM users
			LEFT JOIN users_platby p1 ON up_id_user=u_id
			LEFT JOIN users_skupiny ON users.u_skupina=users_skupiny.us_id
		WHERE u_system='0' AND u_dancer='1' AND u_confirmed='1' AND u_ban='0' " .
			($group > -1 ? "AND u_group='$group' " : '') .
		"AND (p1.up_id IS NULL OR
			(p1.up_plati_do >= CURDATE() AND p1.up_plati_od <= CURDATE()) OR
			(p1.up_placeno = (SELECT MAX(p2.up_placeno) FROM users_platby p2
				WHERE p2.up_id_user=u_id) AND p1.up_plati_do < CURDATE()))
		ORDER BY u_prijmeni");
		return DBUser::getArray($res);
	}
	public static function getGroupCounts() {
		$res = DBUser::query(
		"SELECT u_group,count(*) as count,permissions.*
		FROM users LEFT JOIN permissions ON u_group=pe_id group by u_group");
		return DBUser::getArray($res);
	}
}
?>