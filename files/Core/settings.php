<?php

$real_name = realpath(basename($_SERVER['SCRIPT_NAME']));
$_SERVER['DOCUMENT_ROOT'] = substr($real_name, 0, strpos($real_name, $_SERVER['SCRIPT_NAME']));
unset($real_name);

set_include_path(
	$_SERVER['DOCUMENT_ROOT'] . '/files/Core/database' . PATH_SEPARATOR .
	$_SERVER['DOCUMENT_ROOT'] . '/files/Core/display' . PATH_SEPARATOR . get_include_path());
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

define("HEADER", "files/Static/Header.inc");
define("FOOTER", "files/Static/Footer.inc");
define("LOG", "log/error.log");
define("PHP_LOG", "log/php.log");

ini_set("log_errors" , "1");
ini_set("error_log" , PHP_LOG);
ini_set("display_errors" , "0");

define("DEBUG", "1");
if(DEBUG) {
	ini_set('display_errors','On'); 
	error_reporting(-1);
}

define("L_UNCONFIRMED", "-1");
define("L_HOST", "0");
define("L_USER", "1");
define("L_EDITOR", "5");
define("L_TRENER", "6");
define("L_ADMIN", "50");
define("L_SADMIN", "99");

define("DB_SERVER", "localhost");
define("DB_USER", "root");
define("DB_PASS", "konik1");
define("DB_DATABASE", "olymp");

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

define("C_ZLUTA", "1");
define("C_MODRA", "2");
define("C_CERVENA", "4");

define("CORE", "files/Core");

$sitemap_static = array(
	"home"				=> "files/Main/Home/Main.inc",
	"oklubu"			=> "files/Main/OKlubu/Main.inc",
	"oklubu/historie"	=> "files/Main/OKlubu/Historie.inc",
	"oklubu/treninky"	=> "files/Main/OKlubu/Treninky.inc",
	"oklubu/jine"		=> "files/Main/OKlubu/Jine.inc",
	"aktualne"			=> "files/Main/Aktualne/Main.inc",
	"inzerce"			=> "files/Main/Inzerce/Main.inc",
	"foto"				=> "files/Main/Fotogalerie/Main.inc",
	"nabizime"			=> "files/Main/Nabizime/Main.inc",
	"forum"				=> "files/Main/Forum/Main.inc",
	"kontakt"			=> "files/Main/Kontakt/Main.inc",
	"done"				=> "files/Main/RegistraceDone.inc"
);
$sitemap_dynamic = array(
	"error"					=> "files/Main/Error.inc",
	"logout"				=> "files/Main/Logout.inc",
	"registrace"			=> "files/Main/Registrace.inc",
	"member/download"		=> "files/Member/Download.inc",
//FIXME: Other pages
//FIXME: Přidat třídu, body, žebříček 
	"member/home"			=> "files/Member/Home.inc",
	"member/nastenka"		=> "files/Member/Nastenka.inc",
	"member/rozpis"			=> "files/Member/Rozpis.inc",
	"member/nabidka"		=> "files/Member/Nabidka.inc",
	"member/tas"			=> "files/Member/TaS.inc",
	"member/tas-vypis"		=> "files/Member/TaSVypis.inc",
	"member/dokumenty"		=> "files/Member/Dokumenty.inc",
	"member/zebricek"		=> "files/Member/Zebricek.inc",
	"member/profil"			=> "files/Member/Profil.inc",

	"admin/home"			=> "files/Admin/Main.inc",
	"admin/dokumenty"		=> "files/Admin/Dokumenty.inc",
	"admin/nastenka"		=> "files/Admin/Nastenka.inc",
	"admin/nabidka"			=> "files/Admin/Nabidka.inc",
	"admin/nabidka-detail"	=> "files/Admin/NabidkaDetail.inc",
	"admin/pary"			=> "files/Admin/Pary.inc",
	"admin/rozpis"			=> "files/Admin/Rozpis.inc",
	"admin/rozpis-detail"	=> "files/Admin/RozpisDetail.inc",
	"admin/tas"				=> "files/Admin/TaS.inc",
	"admin/tas-detail"		=> "files/Admin/TaSDetail.inc",
	"admin/tas-dokumenty"	=> "files/Admin/TaSDokumenty.inc",
	"admin/users"			=> "files/Admin/Users.inc"
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
	"0"		=> "Ostatní");

public static $sekce = array(
	"admin"			=> array(
			"nastenka"	=> "Správa nástěnky",
			"users"		=> "Správa uživatelů",
			"tas"		=> "Správa TaS",
			"rozpis"	=> "Správa rozpisů",
			"nabidka"	=> "Správa nabídky",
			"dokumenty"	=> "Správa dokumentů",
			"pary"		=> "Správa párů"
		),
	"aktualne"		=> array(
		),
	"member"		=> array(
			"home"	=> "Přehled",
			"nastenka"	=> "Nástěnka",
			"rozpis"	=> "Rozpis tréninků",
			"nabidka"	=> "Nabídka tréninků",
			"tas"		=> "Tábory a soustředění",
			"dokumenty"	=> "Dokumenty",
			"zebricek"	=> "Žebříček"
		),
	"forum"			=> array(
		),
	"foto"		=> array(
		),
	"home"			=> array(
			"zajemci"	=> "Noví zájemci",
			"rada"		=> "Rada klubu",
			"historie"	=> "Historie",
			"mcr"		=> "Mistrovství ČR",
			"liga"		=> "Taneční liga",
			"odkazy"	=> "Odkazy"
		),
	"inzerce"		=> array(
		),
	"kontakt"		=> array(
		),
	"oklubu"		=> array(
			"treneri"	=> "Trenéři"
		),
	"nabizime"		=> array(
		)
);
}
?>
