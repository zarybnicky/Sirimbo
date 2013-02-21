<?php
class Permissions {
	public static function get($module) {
		return User::getPermissions($module);
	}
	
	public static function check($module, $level, $vlastnik = null) {
		$l = User::getPermissions($module);
		if($l == P_OWNED && $level == P_OWNED && $vlastnik != null)
			return User::getUserID() == $vlastnik;
		return $l >= $level;
	}
	public static function checkError($module, $level, $redirect = null, $vlastnik = null) {
		if(!Permissions::check($module, $level, $vlastnik)) {
			if($redirect !== null) {
				View::redirect($redirect);
			} elseif(User::isLogged()) {
				View::viewError(ER_AUTHORIZATION);
			} else {
				View::redirect('/login?return=' . Request::getURI(), 'Nemáte dostatečná oprávnění k zobrazení požadovaného obsahu');
			}
		}
		return true;
	}
}