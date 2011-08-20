<?php
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
	"error"				=> "files/Main/Error.inc",
//FIXME: Other pages
	"member/home"		=> "files/Member/Home.inc",
	"member/nastenka"	=> "files/Member/Nastenka.inc",
	"member/rozpis"		=> "files/Member/Rozpis.inc",
	"member/nabidka"	=> "files/Member/Nabidka.inc",
	"member/tabory"		=> "files/Member/Home.inc",
	"member/dokumenty"	=> "files/Member/Home.inc",
	"member/zebricek"	=> "files/Member/Home.inc",
	"member/profil"		=> "files/Member/Home.inc",
	"member/logout"		=> "files/Member/Logout.inc",
	"admin/nastenka"	=> "files/Admin/Nastenka.inc",
	"admin/users"		=> "files/Admin/Users.inc",
	"admin/rozpis"		=> "files/Admin/Rozpis.inc",
	"admin/nabidka"		=> "files/Admin/Nabidka.inc"
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
}
?>