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
		if(Permissions::check($module, $level, $vlastnik))
			return true;
		
		if($redirect !== null) {
			Helper::get()->redirect()->sendRedirect($redirect);
		} elseif(User::isLogged()) {
			throw new Exception("Máte nedostatečnou autorizaci pro tuto akci!");
		} else {
			Helper::get()->redirect()->sendRedirect('/login?return=' . Request::getURI(),
				'Nemáte dostatečná oprávnění k zobrazení požadovaného obsahu');
		}
	}
}