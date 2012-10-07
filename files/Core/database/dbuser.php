<?php
class DBUser extends Database {
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
	
	public static function getUserlevel($id) {
		list($id) = DBUser::escapeArray(array($id));
		
		$res = DBUser::query("SELECT u_level FROM users WHERE " .
			"u_id='$id'");
		if(!$res) {
			return false;
		} else {
			$row = DBUser::getSingleRow($res);
			return $row["u_level"];
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
		"SELECT *
		FROM users
		WHERE u_id='$id'");
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
	public static function addTemporaryUser($login, $jmeno, $prijmeni, $permissions) {
		list($login, $jmeno, $prijmeni, $permissions) =
			DBUser::escapeArray(array($login, $jmeno, $prijmeni, $permissions));
		
		DBUser::query(
		"INSERT INTO users
			(u_login,u_pass,u_jmeno,u_prijmeni,u_confirmed,u_temporary,u_level)
		VALUES ('$login','','$jmeno','$prijmeni','1','1','$permissions')");
		$user_id = mysql_insert_id();
		
		DBUser::query("INSERT INTO pary (p_id_partner) VALUES ('" . $user_id . "')");
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
	
	public static function confirmUser($id, $level = "host", $dancer = 0) {
		list($id, $level, $dancer) = DBUser::escapeArray(array($id, $level, $dancer));
		
		switch($level) {
			case 'host': $level = L_HOST; break;
			case 'user': $level = L_USER; break;
			case 'editor': $level = L_EDITOR; break;
			case 'trener': $level = L_TRENER; break;
			case 'admin': $level = L_ADMIN; break;
			default: $level = L_HOST; break;
		}
		
		DBUser::query("UPDATE users SET u_confirmed='1',u_level='$level',u_dancer='$dancer' WHERE u_id='$id'");
	}
	
	public static function setPassword($id, $passwd) {
		list($id, $passwd) = DBUser::escapeArray(array($id, $passwd));
		
		DBUser::query("UPDATE users SET u_pass='$passwd' WHERE u_id='$id'");
		return true;
	}
	
	public static function setUserData($id, $jmeno, $prijmeni, $pohlavi, $email, $telefon,
			$poznamky, $level, $dancer, $lock, $ban, $system) {
		list($id, $jmeno, $prijmeni, $pohlavi, $email, $telefon, $poznamky, $level, $dancer,
			$lock, $ban, $system) = DBUser::escapeArray(array($id, $jmeno, $prijmeni, $pohlavi,
			$email, $telefon, $poznamky, $level, $dancer, $lock, $ban, $system));
			
		DBUser::query("UPDATE users SET " .
			"u_jmeno='$jmeno',u_prijmeni='$prijmeni',u_pohlavi='$pohlavi',u_email='$email'," .
			"u_telefon='$telefon',u_poznamky='$poznamky',u_level='$level',u_dancer='$dancer',u_lock='$lock'," .
			"u_ban='$ban',u_system='$system' WHERE u_id='$id'");
		return true;	
	}
	
	public static function addUser($login, $pass, $jmeno, $prijmeni, $pohlavi, $email, $telefon,
			$poznamky, $level, $dancer, $lock, $ban, $confirmed, $system) {
		
		list($login, $pass, $jmeno, $prijmeni, $pohlavi, $email, $telefon, $poznamky, $level,
			$dancer, $lock, $ban, $confirmed, $system) = DBUser::escapeArray(array($login, $pass,
			$jmeno, $prijmeni, $pohlavi, $email, $telefon, $poznamky, $level, $dancer, $lock,
			$ban, $confirmed, $system));
		
		DBUser::query("INSERT INTO users " .
			"(u_login,u_pass,u_jmeno,u_prijmeni,u_pohlavi,u_email,u_telefon," .
			"u_poznamky,u_level,u_dancer,u_lock,u_ban,u_confirmed,u_system) VALUES " .
			"('$login','$pass','$jmeno','$prijmeni','$pohlavi','$email','$telefon'," .
			"'$poznamky','$level','$dancer','$lock','$ban','$confirmed','$system')");
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
		DBUser::query(
		"DELETE FROM pary WHERE p_id_partner='$id' AND p_archiv='0'");
		
		return true;
	}
	
	public static function getUsers($level = NULL) {
		if($level == NULL)
			$res = DBUser::query("SELECT * FROM users ORDER BY u_prijmeni");
		else
			$res = DBUser::query("SELECT * FROM users WHERE u_level='$level' ORDER BY u_prijmeni");
		return DBUser::getArray($res);
	}
	
	public static function getUsersByPohlavi($pohlavi) {
		list($pohlavi) = DBUser::escapeArray(array($pohlavi));
		
		$res = DBUser::query("SELECT * FROM users WHERE u_pohlavi='$pohlavi' ORDER BY u_prijmeni");
		return DBUser::getArray($res);
	}
	
	public static function getNewUsers() {
		$res = DBUser::query("SELECT * FROM users WHERE u_confirmed='0' ORDER BY u_prijmeni");
		return DBUser::getArray($res);
	}
	
	public static function getActiveUsers($level = -1) {
		$res = DBUser::query("SELECT * FROM users WHERE u_system='0' AND u_confirmed='1' AND u_ban='0' " .
			($level > -1 ? "AND u_level='$level' " : '') .
			"ORDER BY u_prijmeni ");
		return DBUser::getArray($res);
	}
	
	public static function getActiveDancers($level = -1) {
		$res = DBUser::query("SELECT * FROM users WHERE u_system='0' AND u_dancer='1' AND " .
			"u_confirmed='1' AND u_ban='0' " .
			($level > -1 ? "AND u_level='$level' " : '') .
			"ORDER BY u_prijmeni");
		return DBUser::getArray($res);
	}
	
	public static function getTrener() {
		$res = DBUser::query("SELECT * FROM users WHERE u_level>=" . L_TRENER .
			" ORDER BY u_prijmeni");
		return DBUser::getArray($res);
	}
	
	public static function getMembers() {
		$res = DBUser::query("SELECT * FROM users WHERE u_level>=" . L_USER .
			" ORDER BY u_prijmeni");
		return DBUser::getArray($res);
	}
}
?>