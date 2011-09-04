<?php

set_include_path($_SERVER['DOCUMENT_ROOT'] . '/files/Core/database' . PATH_SEPARATOR . get_include_path());
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

define("DEBUG", "1");
if(DEBUG) {
	ini_set('display_errors','On'); 
	error_reporting(-1);
}

define("HEADER", "files/Static/Header.inc");
define("FOOTER", "files/Static/Footer.inc");
define("LOG", "log/error.log");
define("PHP_LOG", "log/php.log");

ini_set("log_errors" , "1");
ini_set("error_log" , PHP_LOG);
ini_set("display_errors" , "0");

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
define("ER_NOT_FOUND_RIGHT", "not_found_right");
define("ER_NOT_POSSIBLE", "not_possible");
define("ER_SCRIPT_FATAL", "script_fatal");

define("CORE", "files/Core");

$sitemap_static = array(
	"home"				=> "files/Main/Home.inc",
	"onas"				=> "files/Main/ONas.inc",
	"onas/historie"		=> "files/Main/ONas/Historie.inc",
	"onas/treninky"		=> "files/Main/ONas/Treninky.inc",
	"onas/jine"			=> "files/Main/ONas/Jine.inc",
	"nabor"				=> "files/Main/Nabor.inc",
	"ukazky"			=> "files/Main/Ukazky.inc",
	"foto"				=> "files/Main/Fotogalerie.inc",
	"pary"				=> "files/Main/Pary.inc",
	"forum"				=> "files/Main/Forum.inc",
	"inzerce"			=> "files/Main/Inzerce.inc"
);
$sitemap_dynamic = array(
	"error"					=> "files/Main/Error.inc",
//FIXME: Other pages
	"member/home"			=> "files/Member/Home.inc",
	"member/nastenka"		=> "files/Member/Nastenka.inc",
	"member/rozpis"			=> "files/Member/Rozpis.inc",
	"member/nabidka"		=> "files/Member/Nabidka.inc",
	"member/tabory"			=> "files/Member/Home.inc",
	"member/dokumenty"		=> "files/Member/Dokumenty.inc",
	"member/zebricek"		=> "files/Member/Home.inc",
	"member/profil"			=> "files/Member/Home.inc",
	"member/logout"			=> "files/Member/Logout.inc",
	"admin/nastenka"		=> "files/Admin/Nastenka.inc",
	"admin/users"			=> "files/Admin/Users.inc",
	"admin/rozpis"			=> "files/Admin/Rozpis.inc",
	"admin/rozpis-detail"	=> "files/Admin/RozpisDetail.inc",
	"admin/nabidka"			=> "files/Admin/Nabidka.inc",
	"admin/nabidka-detail"	=> "files/Admin/NabidkaDetail.inc",
	"admin/dokumenty"		=> "files/Admin/Dokumenty.inc"
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
	"not_found_right"		=> "files/Error/NotFoundRight.inc",
	"not_posible"			=> "files/Error/NotPossible.inc",
	"script_fatal"			=> "files/Error/ScriptFatal.inc"
);

public static $document_types = array(
	"1"		=> "Schůze, rady",
	"2"		=> "Soutěže",
	"3"		=> "Tábory",
	"0"		=> "Ostatní");
}
?>