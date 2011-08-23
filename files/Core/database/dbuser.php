<?php
class DBUser extends Database {
public static function checkUser($login, $pass) {
		list($login, $pass) = DBUser::escapeArray(array($login, $pass));
		
		$res = DBUser::query("SELECT COUNT(*) FROM users WHERE " .
			"u_login='$login' AND u_pass='$pass'");
		if(!$res) {
			return false;
		} else {
			$row = DBUser::getSingleRow($res);
			return $row["COUNT(*)"];
		}
	}
	
	public static function getUserlevel($login) {
		list($login) = DBUser::escapeArray(array($login));
		
		$res = DBUser::query("SELECT u_level FROM users WHERE " .
			"u_login='$login'");
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
	
	public static function getUserData($login) {
		list($login) = DBUser::escapeArray(array($login));
		
		$res = DBUser::query("SELECT u_login,u_jmeno,u_pass,u_jmeno,u_prijmeni," .
			"u_email,u_telefon,u_poznamky,u_level,u_ban,u_lock FROM users WHERE u_login='$login'");
		if(!$res) {
			return false;
		} else {
			return DBUser::getSingleRow($res);
		}
	}
	
	public static function isUserBanned($login) {
		list($login) = DBUser::escapeArray(array($login));
		
		$res = DBUser::query("SELECT u_ban FROM users WHERE u_login='$login'");
		if(!$res) {
			return false;
		} else {
			$row = DBUser::getSingleRow($res);
			return $row["u_ban"];
		}
	}
	
	public static function isUserLocked($login) {
		list($login) = DBUser::escapeArray(array($login));
		
		$res = DBUser::query("SELECT u_lock FROM users WHERE u_login='$login'");
		if(!$res) {
			return false;
		} else {
			$row = DBUser::getSingleRow($res);
			return $row["u_lock"];
		}
	}
	
	public static function setUserData($login, $level, $lock, $ban, $jmeno = "", $prijmeni = "",
		$email = "", $telefon = "", $poznamky = "") {
		
		list($login, $jmeno, $prijmeni, $email, $telefon, $poznamky, $level, $lock, $ban) =
			DBUser::escapeArray(array($login, $jmeno, $prijmeni, $email, $telefon,
			$poznamky, $level, $lock, $ban));
		
			
		DBUser::query("UPDATE users SET " .
			"u_jmeno='$jmeno',u_prijmeni='$prijmeni',u_email='$email',u_telefon='$telefon'," .
			"u_poznamky='$poznamky',u_level='$level', u_lock='$lock', u_ban='$ban' WHERE u_login='$login'");
		return true;	
	}
	
	public static function addUser($login, $pass, $level, $lock, $ban, $name = "", $surname = "",
		$email = "", $telefon = "", $poznamky = "") {
		
		list($login, $pass, $name, $surname, $email, $telefon, $poznamky, $level, $lock, $ban) =
			DBUser::escapeArray(array($login, $pass, $name, $surname, $email, $telefon,
			$poznamky, $level, $lock, $ban));
		
		DBUser::query("INSERT INTO users " .
			"(u_login,u_pass,u_jmeno,u_prijmeni,u_email,u_telefon," .
			"u_poznamky,u_level,u_lock,u_ban) VALUES " .
			"('$login','$pass','$name','$surname','$email','$telefon'," .
			"'$poznamky','$level','$lock','$ban')");
		return true;
	}
	
	public static function removeUser($login) {
		list($login) = DBUser::escapeArray(array($login));
		
		$res = DBUser::query("SELECT u_id FROM users WHERE u_login='$login'");
		$row = DBUser::getSingleRow($res);
		$id = $row["u_id"];
		DBUser::query("DELETE FROM users WHERE u_id='$id'");
		DBUser::query("DELETE FROM rozpis WHERE r_trener='$id'");
		DBUser::query("DELETE FROM rozpis_item WHERE ri_partner='$id'");
		DBUser::query("DELETE FROM nabidka WHERE n_trener='$id'");
		DBUser::query("DELETE FROM nabidka_item WHERE ni_partner='$id'");
		DBUser::query("DELETE FROM upozorneni WHERE up_kdo='$id'");
		return true;
	}
	
	public static function getUsers() {
		$res = DBUser::query("SELECT * FROM users");
		return DBUser::getArray($res);
	}
	
	public static function getUserNames() {
		$res = DBUser::query("SELECT u_id,u_login,u_jmeno,u_prijmeni FROM users");
		return DBUser::getArray($res);
	}
	
	public static function getTrener() {
		$res = DBUser::query("SELECT u_id,u_jmeno,u_prijmeni FROM users WHERE u_level>=" . L_TRENER);
		return DBUser::getArray($res);
	}
}
?>