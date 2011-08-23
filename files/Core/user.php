<?php
class User {
	public static function login($login, $pass) {
		if(($login == "superadmin" && $pass == "17c4520f6cfd1ab53d8745e84681eb49")
				|| DBUser::checkUser($login, $pass)) {
			if(DBUser::isUserBanned($login))
				View::viewError(ER_BAN);
			$_SESSION["login"] = 1;
			$_SESSION["user"] = $login;
			User::loadUser($login);
			return true;
		} else
			return false;
	}
	
	public static function logout() {
		session_unset();
	}
	
	public static function loadUser($username) {
		if($username == "superadmin" && $_SESSION["user"] == "superadmin")
			$_SESSION["level"] = L_SADMIN;
		else
			$_SESSION["level"] = DBUser::getUserlevel($username);
		$_SESSION["id"] = DBUser::getUserID($username);
		return true;
	}
	
	public static function checkPermissionsError($level) {
		if(!isset($_SESSION["level"]) || $_SESSION["level"] < $level)
			View::viewError(ER_AUTHORIZATION);
	}
	
	public static function checkPermissionsBool($level) {
		if(!isset($_SESSION["level"]) || $_SESSION["level"] < $level)
			return false;
		return true;
	}
	
	public static function getUserID() {
		return $_SESSION["id"];
	}
	
	public static function getUserName() {
		return $_SESSION["user"];
	}
	
	public static function getUserLevel() {
		return $_SESSION["level"];
	}
	
	public static function isLogged() {
		if(isset($_SESSION["login"]) && $_SESSION["login"] === 1)
			return true;
		return false;
	}
	
	public static function addUser($login, $pass, $level, $lock, $ban, $name = "",
		$surname = "", $email = "", $telefon = "", $poznamky = "") {
		DBUser::addUser($login, $pass, $name, $surname, $email, $telefon,
			$poznamky, $level, $lock, $ban);
		return true;
	}
	
	public static function editUser($login, $level, $lock, $ban, $name = "",
		$surname = "", $email = "", $telefon = "", $poznamky = "") {
		DBUser::setUserData($login, $name, $surname, $email, $telefon,
			$poznamky, $level, $lock, $ban);
		return true;
	}
}

session_start();
session_regenerate_id();
if(isset($_SESSION["user"]))
	User::loadUser($_SESSION["user"]);
?>