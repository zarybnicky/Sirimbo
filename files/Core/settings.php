<?php
$real_name = realpath(basename($_SERVER['SCRIPT_NAME']));
$_SERVER['DOCUMENT_ROOT'] = substr($real_name, 0, strpos($real_name, basename($_SERVER['SCRIPT_NAME'])) - 1);
unset($real_name);

define('ROOT', $_SERVER['DOCUMENT_ROOT']);
define('FILES', ROOT . DIRECTORY_SEPARATOR . 'files');
define('CORE', FILES . DIRECTORY_SEPARATOR . 'Core');
define('ERROR', FILES . DIRECTORY_SEPARATOR . 'Error');
define('SETTINGS', CORE . DIRECTORY_SEPARATOR . 'settings');
define('GALERIE', ROOT . DIRECTORY_SEPARATOR . 'galerie');
define('HEADER', FILES . DIRECTORY_SEPARATOR . 'Static' . DIRECTORY_SEPARATOR . 'Header.inc');
define('FOOTER', FILES . DIRECTORY_SEPARATOR . 'Static' . DIRECTORY_SEPARATOR . 'Footer.inc');
define('HEADER_TISK', FILES . DIRECTORY_SEPARATOR . 'Static' . DIRECTORY_SEPARATOR . 'HeaderTisk.inc');
define('FOOTER_TISK', FILES . DIRECTORY_SEPARATOR . 'Static' . DIRECTORY_SEPARATOR . 'FooterTisk.inc');
define('LOG', ROOT . DIRECTORY_SEPARATOR . 'log' . DIRECTORY_SEPARATOR . 'error.log');
define('PHP_LOG', ROOT . 'log' . DIRECTORY_SEPARATOR . 'php.log');

set_include_path(
	CORE . DIRECTORY_SEPARATOR . 'database' . PATH_SEPARATOR .
	CORE . DIRECTORY_SEPARATOR . 'display' . PATH_SEPARATOR .
	CORE . DIRECTORY_SEPARATOR . 'helpers' . PATH_SEPARATOR .
	CORE . DIRECTORY_SEPARATOR . 'paging' . PATH_SEPARATOR .
	CORE . DIRECTORY_SEPARATOR . 'types' . PATH_SEPARATOR .
	get_include_path());
spl_autoload_extensions('.php');
spl_autoload_register();

function _shutdown_handler() {
	if (($error = error_get_last())) {
		if($error['type'] == E_ERROR || $error['type'] == E_RECOVERABLE_ERROR) {
			ob_clean();
			Log::write($error['message']);
			header('Location: /error?id=script_fatal');
		}
	}
}
function _exception_handler($severity, $message, $filepath, $line) {
	if ($severity == E_STRICT) {
		return;
	}
	log_exception($severity, $message, $filepath, $line);
	return true;
}

register_shutdown_function('_shutdown_handler');
//set_error_handler('_exception_handler');

date_default_timezone_set('Europe/Paris');

define('DEFAULT_FROM_MAIL', 'TK Olymp.cz <noreply@tkolymp.cz>');
define('DEFAULT_ADMIN_MAIL', 'tkolymp@tkolymp.cz');

//ini_set('log_errors' , '1');
//ini_set('error_log' , PHP_LOG);
//ini_set('display_errors' , '0');

define('DEBUG', '1');
if(DEBUG) {
	//ini_set('display_errors','On'); 
	error_reporting(-1);
}

include (SETTINGS . DIRECTORY_SEPARATOR . 'db.php');
include (SETTINGS . DIRECTORY_SEPARATOR . 'sitemap.php');

define('NABOR', '0');

//-----Ciselne hodnoty urovni uzivatelu-----//
define('L_ALL', '-1');
define('L_UNCONFIRMED', '-1');
define('L_HOST', '0');
define('L_USER', '1');
define('L_EDITOR', '5');
define('L_TRENER', '6');
define('L_ADMIN', '50');
define('L_SADMIN', '99');

//-----Hodnoceni paru-----//
define('AMEND_Z', '0.2');
define('AMEND_H', '0.5');
define('AMEND_D', '1.0');
define('AMEND_C', '1.6');
define('AMEND_B', '2.1');
define('AMEND_A', '2.7');
define('AMEND_M', '3.4');

define('BONUS_Z', '0');
define('BONUS_H', '80');	//400*AMEND_Z + BONUS_Z
define('BONUS_D', '280');	//400*AMEND_H + BONUS_H
define('BONUS_C', '680');	//400*AMEND_D + BONUS_D
define('BONUS_B', '1320');	//400*AMEND_C + BONUS_C
define('BONUS_A', '2160');	//400*AMEND_B + BONUS_B
define('BONUS_M', '3240');	//400*AMEND_A + BONUS_A

//-----Aliasy chyb-----//
define('ER_AUTHORIZATION', 'authorization');
define('ER_CORRUPT_DATA', 'corrupt_data');
define('ER_BAN', 'ban');
define('ER_DATABASE', 'database');
define('ER_DATABASE_CONNECTION', 'database_connection');
define('ER_PREPARING_FILE', 'preparing_file');
define('ER_CORRUPT_KEY_FILE', 'corrupt_key_file');
define('ER_NOT_APPROVED', 'not_approved');
define('ER_NOT_FOUND_RIGHT', 'not_found_right');
define('ER_NOT_POSSIBLE', 'not_possible');
define('ER_SCRIPT_FATAL', 'script_fatal');

//-----Barvy pro nastenku-----//
define('C_ZLUTA', '1');
define('C_MODRA', '2');
define('C_CERVENA', '4');

define('NOVINKY_COUNT', 10);
define('INZERCE_COUNT', 10);
define('INZERCE_ALL', 0);
define('INZERCE_PRODAM', 1);
define('INZERCE_KOUPIM', 2);
define('INZERCE_PARTNER', 3);
define('INZERCE_PARTNERKA', 4);
define('AKTUALITY_CLANKY', 1);
define('AKTUALITY_VIDEA', 2);
define('AKTUALITY_KRATKE', 3);
define('AKTUALITY_PREVIEW', 200);
define('THUMBNAIL_MAX', 150);

define('P_NONE', 1);
define('P_VIEW', 2);
define('P_MEMBER', 4);
define('P_OWNED', 8);
define('P_ADMIN', 16);

class Settings {
public static $errors = array(
	'authorization'			=> 'files/Error/Authorization.inc',
	'corrupt_data'			=> 'files/Error/CorruptData.inc',
	'ban'					=> 'files/Error/Ban.inc',
	'database'				=> 'files/Error/Database.inc',
	'database_connection'	=> 'files/Error/DatabaseConnection.inc',
	'preparing_file'		=> 'files/Error/InPreparation.inc',
	'corrupt_key_file'		=> 'files/Error/KeyFileCorrupt.inc',
	'not_approved'			=> 'files/Error/NotApproved.inc',
	'not_found_right'		=> 'files/Error/NotFoundRight.inc',
	'not_posible'			=> 'files/Error/NotPossible.inc',
	'script_fatal'			=> 'files/Error/ScriptFatal.inc'
);

public static $document_types = array(
	'1'		=> 'Schůze, rady',
	'2'		=> 'Soutěže',
	'3'		=> 'Tábory',
	'4'		=> 'Inzerce',
	'0'		=> 'Ostatní'
);

public static $barvy = array(
	'white'		=> array('bílá',		'#FFF'),
	'yellow'	=> array('žlutá',		'#FF0'),
	'orange'	=> array('oranžová',	'#F90'),
	'red'		=> array('červená',		'#F00'),
	'violet'	=> array('fialová',		'#606'),
	'blue'		=> array('modrá',		'#00F'),
	'lightblue'	=> array('světle modrá','#0BD'),
	'aqua'		=> array('modrozelená',	'#2A9'),
	'bahia'		=> array('zelenomodrá',	'#AD1'),
	'green'		=> array('zelená',		'#0F0'),
	'darkgreen'	=> array('tmavě zelená','#5A4'),
	'brown'		=> array('hnědá',		'#630'),
	'black'		=> array('černá',		'#000'),
	'pink'		=> array('růžová',		'#F09'),
	'grey'		=> array('šedá',		'#999')
);

public static $platby_obdobi = array(
	'1-pololeti'	=> array('-09-01', '-01-31', '1. pololetí - 1.9. - 31.1.', '1. pololetí'),
	'2-pololeti'	=> array('-02-01', '-06-30', '2. pololetí - 1.2. - 30.6.', '2. pololetí'),
	'1-ctvrtleti'	=> array('-09-01', '-11-15', '1. čtvrtletí - 1.9. - 15.11.', '1. čtvrtletí'),
	'2-ctvrtleti'	=> array('-11-16', '-01-31', '2. čtvrtletí - 16.11. - 31.1.', '2. čtvrtletí'),
	'3-ctvrtleti'	=> array('-02-01', '-04-15', '3. čtvrtletí - 1.2. - 15.4.', '3. čtvrtletí'),
	'4-ctvrtleti'	=> array('-04-16', '-06-30', '4. čtvrtletí - 1.9. - 30.6.', '4. čtvrtletí')
);

public static $permission_levels = array(
	P_NONE => 'Bez přístupu',
	P_VIEW => 'Zobrazit',
	P_MEMBER => 'Upravit',
	P_OWNED => 'Admin (svoje)',
	P_ADMIN => 'Admin'
);

public static $permissions = array(
	'akce' => array(
		'name' => "Akce",
		'default' => P_MEMBER,
		P_NONE => 1, P_VIEW => 1, P_MEMBER => 1, P_OWNED => 1, P_ADMIN => 1),
	'aktuality' => array(
		'name' => "Aktuality",
		'default' => P_VIEW,
		P_VIEW => 1, P_OWNED => 1, P_ADMIN => 1),
	'ankety' => array(
		'name' => "Ankety",
		'default' => P_VIEW,
		P_VIEW => 1, P_OWNED => 1, P_ADMIN => 1),
	'dokumenty' => array(
		'name' => "Dokumenty",
		'default' => P_MEMBER,
		P_NONE => 1, P_MEMBER => 1, P_OWNED => 1, P_ADMIN => 1),
	'galerie' => array(
		'name' => "Fotogalerie",
		'default' => P_VIEW,
		P_VIEW => 1, P_OWNED => 1, P_ADMIN => 1),
	'inzerce' => array(
		'name' => "Inzerce",
		'default' => P_MEMBER,
		P_VIEW => 1, P_MEMBER => 1, P_ADMIN => 1),
	'konzole' => array(
		'name' => "Konzole",
		'default' => P_NONE,
		P_NONE => 1, P_ADMIN => 1),
	'nabidka' => array(
		'name' => "Nabídka",
		'default' => P_MEMBER,
		P_NONE => 1, P_VIEW => 1, P_MEMBER => 1, P_OWNED => 1, P_ADMIN => 1),
	'nastenka' => array(
		'name' => "Nástěnka",
		'default' => P_VIEW,
		P_NONE => 1, P_VIEW => 1, P_OWNED => 1, P_ADMIN => 1),
	'novinky' => array(
		'name' => "Novinky",
		'default' => P_VIEW,
		P_NONE => 1, P_VIEW => 1, P_OWNED => 1, P_ADMIN => 1),
	'pary' => array(
		'name' => "Páry",
		'default' => P_VIEW,
		P_NONE => 1, P_VIEW => 1, P_ADMIN => 1),
	'platby' => array(
		'name' => "Platby",
		'default' => P_NONE,
		P_NONE => 1, P_ADMIN => 1),
	'permissions' => array(
		'name' => "Oprávnění",
		'default' => P_NONE,
		P_NONE => 1, P_ADMIN => 1),
	'rozpis' => array(
		'name' => "Rozpis",
		'default' => P_MEMBER,
		P_NONE => 1, P_VIEW => 1, P_MEMBER => 1, P_OWNED => 1, P_ADMIN => 1),
	'skupiny' => array(
		'name' => "Skupiny",
		'default' => P_VIEW,
		P_NONE => 1, P_VIEW => 1, P_ADMIN => 1),
	'users' => array(
		'name' => "Uživatelé",
		'default' => P_VIEW,
		P_NONE => 1, P_VIEW => 1, P_ADMIN => 1	),
	'main' => array(
		'name' => "Veřejná část",
		'default' => P_VIEW,
		P_VIEW => 1)
);

public static $sekce = array(
	'admin'		=> array(
			'users/*'		=> array('Správa uživatelů', L_ADMIN),
			'skupiny/*'		=> array('Správa skupin', L_ADMIN),
			'platby/*'		=> array('Správa plateb', L_ADMIN),
			'pary/*'		=> array('Správa párů', L_ADMIN),
			'aktuality/*'	=> array('Správa článků', L_EDITOR),
			'nastenka/*'	=> array('Správa nástěnky', L_EDITOR),
			'rozpis/*'		=> array('Správa rozpisů', L_TRENER),
			'nabidka/*'		=> array('Správa nabídky', L_TRENER),
			'akce/*'		=> array('Správa akcí', L_ADMIN),
			'inzerce/*'		=> array('Správa inzerce', L_ADMIN),
			'galerie/*'		=> array('Správa galerie', L_ADMIN),
			'dokumenty/*'	=> array('Správa dokumentů', L_EDITOR),
			'ankety/*'		=> array('Správa anket', L_EDITOR),
			'permissions/*'	=> array('Správa oprávnění', L_ADMIN),
			'konzole/*'		=> array('Konzole', L_ADMIN)
		),
	'aktuality'	=> array(
			'posledni'		=> 'Nejnovější články',
			'videa'			=> 'Videa',
			'clanky'		=> 'Články',
			'kratke-zpravy'	=> 'Krátké zprávy'
		),
	'member'	=> array(
			'home'		=> 'Novinky',
			'nastenka'	=> 'Nástěnka',
			'rozpis'	=> 'Rozpis tréninků',
			'nabidka'	=> 'Nabídka tréninků',
			'akce'		=> 'Klubové akce',
			'dokumenty'	=> 'Dokumenty',
			'zebricek'	=> 'Žebříček',
			'clenove'	=> 'Členové',
			'profil/*'	=> 'Profil'
		),
	'forum'		=> array(
		),
	'fotogalerie'	=> array(
		),
	'home'		=> array(
			'zajemci'	=> 'Noví zájemci',
			'ankety'	=> 'Ankety',
			'odkazy'	=> 'Odkazy'
		),
	'inzerce'	=> array(
			'posledni'	=> 'Nejnovější inzeráty',
			'prodam'	=> 'Prodám',
			'koupim'	=> 'Koupím',
			'partner'	=> 'Hledám partnera',
			'partnerka'	=> 'Hledám partnerku',
			'add'		=> 'Nový inzerát'
		),
	'kontakt'	=> array(
		),
	'oklubu'	=> array(
			'historie'		=> 'Historie',
			'uspechy'		=> 'Úspěchy v číslech',
			'mistrovstvi'	=> 'Mistrovství ČR',
			'druzstva'		=> 'Družstva',
			'liga'			=> 'Taneční liga',
			'treneri/klubovi'	=> 'Kluboví trenéři',
			'treneri/externi'	=> 'Externí trenéři',
			'saly'			=> 'Kde trénujeme',
			'stanovy.pdf'		=> 'Stanovy klubu'
		),
	'nabizime'	=> array(
			''				=> 'Nabízíme',
			'vystoupeni'	=> 'Taneční vystoupení',
			'individualky'	=> 'Individuální lekce',
			'seminare'		=> 'Skupinové semináře',
			'soustredeni'	=> 'Taneční soustředění'
		)
);
public static $foto_types = array(
	'image/pjpeg' => 'jpg',
	'image/jpeg' => 'jpg',
	'image/gif' => 'gif',
	'image/bmp' => 'bmp',
	'image/x-png' => 'png'
);
public static $gd_function_suffix = array(        
	'image/pjpeg' => 'JPEG',
	'image/jpeg' => 'JPEG', 
	'image/gif' => 'GIF', 
	'image/bmp' => 'BMP',
	'image/x-png' => 'PNG'
);  

public static $no_headers = array(
	'files/Main/Nabor/Main.inc' => ''
);
}?>