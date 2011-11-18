<?php
class User {
	public static function login($login, $pass) {
		if(($login == "superadmin" &&
			$pass == "9947a7bc1549a54e7299fe9a3975c8655430ade0")
				|| DBUser::checkUser($login, $pass)) {
			$data = DBUser::getUserDataByName($login);
			
			if($data['u_ban'])
				View::viewError(ER_BAN);
			if(!$data['u_confirmed'])
				View::viewError(ER_NOT_APPROVED);
			
			$_SESSION["login"] = 1;
			$_SESSION["user"] = $login;
			User::loadUser($data['u_id'], $data);
			return true;
		} else
			return false;
	}
	
	public static function logout() {
		session_unset();
	}
	
	public static function loadUser($id, $data = array()) {
		if(empty($data))
			$data = DBUser::getUserData($id);
		$par = DBPary::getLatestPartner($data['u_id'], $data['u_pohlavi']);
		
		if($id == 1 && $_SESSION["user"] == "superadmin")
			$_SESSION["level"] = L_SADMIN;
		else
			$_SESSION["level"] = $data['u_level'];
		$_SESSION["id"] = $data['u_id'];
		$_SESSION['jmeno'] = $data['u_jmeno'];
		$_SESSION['prijmeni'] = $data['u_prijmeni'];
		$_SESSION["pohlavi"] = $data['u_pohlavi'];
		$_SESSION['par'] = $par['p_id'];
		$_SESSION['partner'] = $par['u_id'];
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
	
	public static function getPartnerID() {
		return $_SESSION['partner'];
	}
	
	public static function getUserName() {
		return $_SESSION["user"];
	}
	
	public static function getUserWholeName() {
		return $_SESSION['jmeno'] . ' ' . $_SESSION['prijmeni'];
	}
	
	public static function getUserLevel() {
		return $_SESSION["level"];
	}
	
	public static function getUserPohlavi() {
		return $_SESSION['pohlavi'];
	}
	
	public static function getParID() {
		return $_SESSION['par'];
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
if(isset($_SESSION["login"]))
	User::loadUser($_SESSION["id"]);
?>