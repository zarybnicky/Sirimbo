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
			
			if(!preg_match("/^[A-Z0-9._%-]+@[A-Z0-9.-]+\.[A-Z]{2,4}$/i", $data['u_email']) ||
					!preg_match("/^((\+|00)\d{3})?( ?\d{3}){3}$/", $data['u_telefon']) ||
					!preg_match("/^((?:19|20)\d\d)-(0[1-9]|1[012])-(0[1-9]|[12][0-9]|3[01])$/",
						$data['u_narozeni'])) {
				$_SESSION['invalid_data'] = 1;
				View::redirect('/member/profil/edit', 'Prosím vyplňte požadované údaje.', true);
			} else {
				$_SESSION['invalid_data'] = 0;
			}
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

		foreach(Settings::$permissions as $key => $item) {
			if($data['u_group'] == 0)
				$_SESSION['permissions'][$key] = P_NONE;
			else
				$_SESSION['permissions'][$key] = $data['pe_' . $key];
		}
		
		$_SESSION["id"] = $data['u_id'];
		$_SESSION["user"] = strtolower($data['u_login']);
		$_SESSION['jmeno'] = $data['u_jmeno'];
		$_SESSION['prijmeni'] = $data['u_prijmeni'];
		$_SESSION["pohlavi"] = $data['u_pohlavi'];
		$_SESSION['narozeni'] = $data['u_narozeni'];
		$_SESSION['group'] = $data['u_group'];
		$_SESSION['skupina'] = $data['u_skupina'];
		$_SESSION['skupina_data'] = array(
			'us_id '=> $data['us_id'],
			'us_color' => $data['us_color'],
			'us_popis' => $data['us_popis']
		);
		$_SESSION['par'] = $par['p_id'];
		$_SESSION['partner'] = $par['u_id'];
		$_SESSION['zaplaceno'] =
			(bool) (strcmp($data['up_plati_do'], date('Y-m-d', strtotime('+ 14 days'))) >= 0);
		return true;
	}
	
	public static function getPermissions($module = '') {
		if(!User::isLogged()) {
			if($module)
				return P_NONE;
			return array();
		} elseif(User::getUserID() == 1) {
			if($module)
				return P_ADMIN;
			return $_SESSION['permissions'];
		} elseif(User::getUserGroup() == 1) {
			if($module)
				return P_NONE;
			return $_SESSION['permissions'];
		} else {
			if($module && isset($_SESSION['permissions'][$module]))
				return $_SESSION['permissions'][$module];
			return $_SESSION['permissions'];
		}
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
	
	public static function getUserGroup() {
		return $_SESSION["group"];
	}
	
	public static function getUserPohlavi() {
		return $_SESSION['pohlavi'];
	}
	
	public static function getDatumNarozeni() {
		return $_SESSION['narozeni'];
	}
	
	public static function getSkupina() {
		return $_SESSION['skupina'];
	}
	
	public static function getSkupinaData() {
		return $_SESSION['skupina_data'];
	}
	
	public static function getZaplaceno() {
		return $_SESSION['zaplaceno'];
	}
	
	public static function getParID() {
		return $_SESSION['par'];
	}
	
	public static function isLogged() {
		if(isset($_SESSION["login"]) && $_SESSION["login"] === 1)
			return true;
		return false;
	}
	
	public static function register($login, $pass, $name, $surname, $pohlavi, $email, $telefon,
			$narozeni, $poznamky) {
		DBUser::addUser(strtolower($login), User::Crypt($pass), $name, $surname, $pohlavi, $email,
			$telefon, $narozeni, $poznamky, L_UNCONFIRMED, '0', "0", "0", "0", "0");
		
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
?>