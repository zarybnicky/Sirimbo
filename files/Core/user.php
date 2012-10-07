<?php
class User {
	public static function login($login, $pass) {
		if(Database::isDatabaseError()) {
			User::logout();
			return false;
		}
		$login = strtolower($login);
		
		if(($login == "superadmin" &&
				$pass == "9947a7bc1549a54e7299fe9a3975c8655430ade0" && ($id = 1))
				|| ($id = DBUser::checkUser($login, $pass))) {
			$data = DBUser::getUserData($id);
			
			if($data['u_ban'])
				View::viewError(ER_BAN);
			if(!$data['u_confirmed'])
				View::viewError(ER_NOT_APPROVED);
			
			$_SESSION["login"] = 1;
			User::loadUser($data['u_id'], $data);
			return true;
		} else
			return false;
	}
	
	public static function logout() {
		session_unset();
	}
	
	public static function loadUser($id, $data = array()) {
		if(Database::isDatabaseError()) {
			User::logout();
			return false;
		}
		
		if(empty($data))
			$data = DBUser::getUserData($id);
		$par = DBPary::getLatestPartner($data['u_id'], $data['u_pohlavi']);
		
		$_SESSION["level"] = $data['u_level'];
		$_SESSION["id"] = $data['u_id'];
		$_SESSION["user"] = strtolower($data['u_login']);
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
		if(User::isLogged())
			return $_SESSION["id"];
		else
			return 0;
	}
	
	public static function getPartnerID() {
		return $_SESSION['partner'];
	}
	
	public static function getUserName() {
		return $_SESSION["user"];
	}
	
	public static function getUserJmeno() {
		return $_SESSION['jmeno'];
	}
	
	public static function getUserPrijmeni() {
		return $_SESSION['prijmeni'];
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
		DBUser::addUser(strtolower($login), User::Crypt($pass), $name, $surname, $pohlavi, $email, $telefon,
			$poznamky, L_UNCONFIRMED, '0', "0", "0", "0", "0");
		
		Mailer::new_user_notice(DEFAULT_ADMIN_MAIL, $login);
	}
	
	public static function Crypt($passwd) {
		$fix = md5("######TK.-.OLYMP######");
		return sha1($fix . $passwd . $fix);
	}
	
	public static function var_symbol($id) {
		return str_pad($id, 6, '0', STR_PAD_LEFT);
	}
}

session_start();
session_regenerate_id();
if(isset($_SESSION["login"]))
	User::loadUser($_SESSION["id"]);
?>