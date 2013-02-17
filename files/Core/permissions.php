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
	
	
	public static function canEditAkce() {
		if(User::checkPermissionsBool(L_ADMIN))
			return true;
		else
			return false;
	}
	public static function canEditAnketa($vlastnik) {
		if(User::checkPermissionsBool(L_ADMIN) ||
				(User::checkPermissionsBool(L_EDITOR) && User::getUserID() == $vlastnik))
			return true;
		else
			return false;
	}
	public static function canEditAktualita($vlastnik) {
		if(User::checkPermissionsBool(L_ADMIN) ||
				(User::checkPermissionsBool(L_EDITOR) && User::getUserID() == $vlastnik))
			return true;
		else
			return false;
	}
	public static function canEditDokument($vlastnik) {
		if(User::checkPermissionsBool(L_ADMIN) ||
				(User::checkPermissionsBool(L_EDITOR) && User::getUserID() == $vlastnik))
			return true;
		else
			return false;
	}
	public static function canEditInzerat($vlastnik) {
		if(User::checkPermissionsBool(L_EDITOR) ||
				(User::checkPermissionsBool(L_USER) && User::getUserID() == $vlastnik))
			return true;
		else
			return false;
	}
	public static function canEditNastenka($pridal, $zamek = false) {
		if(User::checkPermissionsBool(L_ADMIN) ||
				(User::checkPermissionsBool(L_EDITOR) && User::getUserID() == $pridal && !$zamek))
			return true;
		else
			return false;
	}
	public static function canEditNabidka($trener) {
		if(User::checkPermissionsBool(L_ADMIN) ||
				(User::checkPermissionsBool(L_TRENER) && User::getUserID() == $trener))
			return true;
		else
			return false;
	}
	public static function canEditRozpis($trener) {
		if(User::checkPermissionsBool(L_ADMIN) ||
				(User::checkPermissionsBool(L_TRENER) && User::getUserID() == $trener))
			return true;
		else
			return false;
	}
}