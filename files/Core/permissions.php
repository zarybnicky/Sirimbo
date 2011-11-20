<?php
class Permissions {
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
	public static function canEditUser($id, $level, $lock) {
		if(User::checkPermissionsBool(L_SADMIN) ||
			(User::checkPermissionsBool(L_ADMIN) &&
				(User::getUserID() == $id || (User::getUserLevel() < $level && !$lock))))
			return true;
		else
			return false;
	}
}