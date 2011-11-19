<?php
$real_name = realpath(basename($_SERVER['SCRIPT_NAME']));
$_SERVER['DOCUMENT_ROOT'] = substr($real_name, 0, strpos($real_name, $_SERVER['SCRIPT_NAME']));
unset($real_name);

set_include_path(
	$_SERVER['DOCUMENT_ROOT'] . '/files/Core/database' . PATH_SEPARATOR .
	$_SERVER['DOCUMENT_ROOT'] . '/files/Core/display' . PATH_SEPARATOR .
	get_include_path());
spl_autoload_extensions(".php");
spl_autoload_register();

function shutdown() {
	if (($error = error_get_last())) {
		if($error["type"] == E_ERROR || $error["type"] == E_RECOVERABLE_ERROR) {
			ob_clean();
			header("Location: /error?id=script_fatal");
		}
	}
}
register_shutdown_function('shutdown');

define("CORE", "files/Core");
define("HEADER", "files/Static/Header.inc");
define("FOOTER", "files/Static/Footer.inc");
define("LOG", "log/error.log");
define("PHP_LOG", "log/php.log");

//ini_set("log_errors" , "1");
//ini_set("error_log" , PHP_LOG);
//ini_set("display_errors" , "0");

define("DEBUG", "1");
if(DEBUG) {
	//ini_set('display_errors','On'); 
	error_reporting(-1);
}

//-----Databazove pripojeni-----//
/*TESTING*/
define("DB_SERVER", "localhost");
define("DB_USER", "root");
define("DB_PASS", "konik1");
define("DB_DATABASE", "olymp");
/**/
/*PRODUCTION
define("DB_SERVER", "mysql.webzdarma.cz");
define("DB_USER", "zarzar");
define("DB_PASS", "695810");
define("DB_DATABASE", "zarzar");
*/

//-----Ciselne hodnoty urovni uzivatelu-----//
define("L_UNCONFIRMED", "-1");
define("L_HOST", "0");
define("L_USER", "1");
define("L_EDITOR", "5");
define("L_TRENER", "6");
define("L_ADMIN", "50");
define("L_SADMIN", "99");

//-----Hodnoceni paru-----//
define("AMEND_Z", "0.2");
define("AMEND_H", "0.5");
define("AMEND_D", "1.0");
define("AMEND_C", "1.6");
define("AMEND_B", "2.1");
define("AMEND_A", "2.7");
define("AMEND_M", "3.4");

define("BONUS_Z", "0");
define("BONUS_H", "80");	//400*AMEND_Z + BONUS_Z
define("BONUS_D", "280");	//400*AMEND_H + BONUS_H
define("BONUS_C", "680");	//400*AMEND_D + BONUS_D
define("BONUS_B", "1320");	//400*AMEND_C + BONUS_C
define("BONUS_A", "2160");	//400*AMEND_B + BONUS_B
define("BONUS_M", "3240");	//400*AMEND_A + BONUS_A

//-----Aliasy chyb-----//
define("ER_AUTHORIZATION", "authorization");
define("ER_CORRUPT_DATA", "corrupt_data");
define("ER_BAN", "ban");
define("ER_DATABASE", "database");
define("ER_DATABASE_CONNECTION", "database_connection");
define("ER_PREPARING_FILE", "preparing_file");
define("ER_CORRUPT_KEY_FILE", "corrupt_key_file");
define("ER_NOT_APPROVED", "not_approved");
define("ER_NOT_FOUND_RIGHT", "not_found_right");
define("ER_NOT_POSSIBLE", "not_possible");
define("ER_SCRIPT_FATAL", "script_fatal");

//-----Barvy pro nastenku-----//
define("C_ZLUTA", "1");
define("C_MODRA", "2");
define("C_CERVENA", "4");

define("NOVINKY_COUNT", 10);
define("INZERCE_COUNT", 10);
define("INZERCE_ALL", 0);
define("INZERCE_PRODAM", 1);
define("INZERCE_KOUPIM", 2);
define("INZERCE_PARTNER", 3);
define("INZERCE_PARTNERKA", 4);

$sitemap_static = array(
	"home"				=> "files/Main/Home/Main.inc",
	"oklubu"			=> "files/Main/OKlubu/Main.inc",
	"oklubu/treneri"	=> "files/Main/OKlubu/Treneri.inc",
	"aktualne"			=> "files/Main/Aktualne/Main.inc",
	"foto"				=> "files/Main/Fotogalerie/Main.inc",
	"nabizime"			=> "files/Main/Nabizime/Main.inc",
	"forum"				=> "files/Main/Forum/Main.inc",
	"kontakt"			=> "files/Main/Kontakt/Main.inc",
	"done"				=> "files/Main/RegistraceDone.inc"
);
$sitemap_dynamic = array(
	"ankety"				=> "files/Main/Ankety.inc",
	"inzerce"				=> "files/Main/Inzerce/Main.inc",
	"inzerce/posledni"		=> "files/Main/Inzerce/Main.inc",
	"inzerce/prodam"		=> "files/Main/Inzerce/Main.inc",
	"inzerce/koupim"		=> "files/Main/Inzerce/Main.inc",
	"inzerce/partner"		=> "files/Main/Inzerce/Main.inc",
	"inzerce/partnerka"		=> "files/Main/Inzerce/Main.inc",
	"inzerce/add"			=> "files/Main/Inzerce/Main.inc",
	"inzerce/edit"			=> "files/Main/Inzerce/Main.inc",
	"inzerce/remove"		=> "files/Main/Inzerce/Main.inc",

	"error"					=> "files/Main/Error.inc",
	"logout"				=> "files/Main/Logout.inc",
	"registrace"			=> "files/Main/Registrace.inc",
	"member/download"		=> "files/Member/Download.inc",
	
	"member/home"			=> "files/Member/Home.inc",
	"member/nastenka"		=> "files/Member/Nastenka.inc",
	"member/rozpis"			=> "files/Member/Rozpis.inc",
	"member/nabidka"		=> "files/Member/Nabidka.inc",
	"member/akce"			=> "files/Member/Akce.inc",
	"member/dokumenty"		=> "files/Member/Dokumenty.inc",
	"member/zebricek"		=> "files/Member/Zebricek.inc",
	"member/pary"			=> "files/Member/Pary.inc",
	"member/profil"			=> "files/Member/Profil.inc",
	"member/profil/edit"	=> "files/Member/Profil.inc",
	"member/profil/heslo"	=> "files/Member/Profil.inc",
	"member/profil/par"		=> "files/Member/Profil.inc",
	"member/profil/par/partner"	=> "files/Member/Profil.inc",
	"member/profil/par/body"	=> "files/Member/Profil.inc",
	"member/profil/par/zadost"	=> "files/Member/Profil.inc",
	"member/profil/inzerce"		=> "files/Member/Profil.inc",
	"member/profil/inzerce/add"	=> "files/Member/Profil.inc",
	"member/profil/inzerce/edit"	=> "files/Member/Profil.inc",
	"member/profil/inzerce/remove"	=> "files/Member/Profil.inc",

	"admin/home"				=> "files/Admin/Main.inc",
	"admin/dokumenty"			=> "files/Admin/Dokumenty.inc",
	"admin/dokumenty/edit"		=> "files/Admin/Dokumenty.inc",
	"admin/nastenka"			=> "files/Admin/Nastenka.inc",
	"admin/nastenka/add"		=> "files/Admin/Nastenka.inc",
	"admin/nastenka/edit"		=> "files/Admin/Nastenka.inc",
	"admin/nabidka"				=> "files/Admin/Nabidka.inc",
	"admin/nabidka/add"			=> "files/Admin/Nabidka.inc",
	"admin/nabidka/edit"		=> "files/Admin/Nabidka.inc",
	"admin/nabidka/detail"		=> "files/Admin/NabidkaDetail.inc",
	"admin/pary"				=> "files/Admin/Pary.inc",
	"admin/pary/edit"			=> "files/Admin/Pary.inc",
	"admin/rozpis"				=> "files/Admin/Rozpis.inc",
	"admin/rozpis/add"			=> "files/Admin/Rozpis.inc",
	"admin/rozpis/edit"			=> "files/Admin/Rozpis.inc",
	"admin/rozpis/detail"		=> "files/Admin/RozpisDetail.inc",
	"admin/akce"				=> "files/Admin/Akce.inc",
	"admin/akce/add"			=> "files/Admin/Akce.inc",
	"admin/akce/edit"			=> "files/Admin/Akce.inc",
	"admin/akce/detail"			=> "files/Admin/AkceDetail.inc",
	"admin/akce/dokumenty"		=> "files/Admin/AkceDokumenty.inc",
	"admin/users"				=> "files/Admin/Users.inc",
	"admin/users/add"			=> "files/Admin/Users.inc",
	"admin/users/edit"			=> "files/Admin/Users.inc",
	"admin/users/new"			=> "files/Admin/Users.inc",
	"admin/ankety"				=> "files/Admin/Ankety.inc",
	"admin/ankety/add"			=> "files/Admin/Ankety.inc",
	"admin/ankety/edit"			=> "files/Admin/Ankety.inc",
	"admin/inzerce"				=> "files/Admin/Inzerce.inc",
	"admin/inzerce/add"			=> "files/Admin/Inzerce.inc",
	"admin/inzerce/edit"		=> "files/Admin/Inzerce.inc",
	"admin/inzerce/new"			=> "files/Admin/Inzerce.inc"
);

class Settings {
public static $errors = array(
	"authorization"			=> "files/Error/Authorization.inc",
	"corrupt_data"			=> "files/Error/CorruptData.inc",
	"ban"					=> "files/Error/Ban.inc",
	"database"				=> "files/Error/Database.inc",
	"database_connection"	=> "files/Error/DatabaseConnection.inc",
	"preparing_file"		=> "files/Error/InPreparation.inc",
	"corrupt_key_file"		=> "files/Error/KeyFileCorrupt.inc",
	"not_approved"			=> "files/Error/NotApproved.inc",
	"not_found_right"		=> "files/Error/NotFoundRight.inc",
	"not_posible"			=> "files/Error/NotPossible.inc",
	"script_fatal"			=> "files/Error/ScriptFatal.inc"
);

public static $document_types = array(
	"1"		=> "Schůze, rady",
	"2"		=> "Soutěže",
	"3"		=> "Tábory",
	"4"		=> "Inzerce",
	"0"		=> "Ostatní"
);

public static $sekce = array(
	"admin"		=> array(
			"nastenka"	=> "Správa nástěnky",
			"users"		=> "Správa uživatelů",
			"akce"		=> "Správa akcí",
			"rozpis"	=> "Správa rozpisů",
			"nabidka"	=> "Správa nabídky",
			"dokumenty"	=> "Správa dokumentů",
			"pary"		=> "Správa párů",
			"ankety"	=> "Správa anket",
			"inzerce"	=> "Správa inzerce"
		),
	"aktualne"	=> array(
		),
	"member"	=> array(
			"home"		=> "Novinky",
			"nastenka"	=> "Nástěnka",
			"rozpis"	=> "Rozpis tréninků",
			"nabidka"	=> "Nabídka tréninků",
			"akce"		=> "Klubové akce",
			"dokumenty"	=> "Dokumenty",
			"zebricek"	=> "Žebříček",
			"profil"	=> "Profil"
		),
	"forum"		=> array(
		),
	"foto"		=> array(
		),
	"home"		=> array(
			"zajemci"	=> "Noví zájemci",
			"rada"		=> "Rada klubu",
			"historie"	=> "Historie",
			"mcr"		=> "Mistrovství ČR",
			"liga"		=> "Taneční liga",
			"odkazy"	=> "Odkazy"
		),
	"inzerce"	=> array(
			"posledni"	=> "Poslední inzeráty",
			"prodam"	=> "Prodám",
			"koupim"	=> "Koupím",
			"partner"	=> "Hledám partnera",
			"partnerka"	=> "Hledím partnerku",
			"add"		=> "Nový inzerát"
		),
	"kontakt"	=> array(
		),
	"oklubu"	=> array(
			"treneri"	=> "Trenéři"
		),
	"nabizime"	=> array(
		)
);
}
?>