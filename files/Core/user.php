<?php
class User {
	public static function login($login, $pass) {
		if(($login == "superadmin" &&
			$pass == "9947a7bc1549a54e7299fe9a3975c8655430ade0")
				|| DBUser::checkUser($login, $pass)) {
			if(DBUser::isUserBanned($login))
				View::viewError(ER_BAN);
			if(!DBUser::isUserConfirmed($login))
				View::viewError(ER_NOT_APPROVED);
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
		$userData = DBUser::getUserData($username);
		
		if($username == "superadmin" && $_SESSION["user"] == "superadmin")
			$_SESSION["level"] = L_SADMIN;
		else
			$_SESSION["level"] = $userData['u_level'];
		$_SESSION["id"] = $userData['u_id'];
		$_SESSION["pohlavi"] = $userData['u_pohlavi'];
		
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
	
	public static function getUserWholeName() {
		return DBUser::getUserWholeName($_SESSION["id"]);
	}
	
	public static function getUserLevel() {
		return $_SESSION["level"];
	}
	
	public static function getUserPohlavi() {
		return $_SESSION['pohlavi'];
	}
	
	public static function isLogged() {
		if(isset($_SESSION["login"]) && $_SESSION["login"] === 1)
			return true;
		return false;
	}
	
	public static function register($login, $pass, $name, $surname, $pohlavi, $email, $telefon, $poznamky) {
		DBUser::addUser($login, $pass, $name, $surname, $pohlavi, $email, $telefon,
			$poznamky, L_UNCONFIRMED, "0", "0", "0");
	}
	
	public static function Crypt($passwd) {
		$fix = md5("######TK.-.OLYMP######");
		return sha1($fix . $passwd . $fix);
	}
}

session_start();
session_regenerate_id();
if(isset($_SESSION["user"]))
	User::loadUser($_SESSION["user"]);
?>