<?php
$real_name = realpath(basename($_SERVER['SCRIPT_NAME']));
$_SERVER['DOCUMENT_ROOT'] = substr($real_name, 0, strpos($real_name, $_SERVER['SCRIPT_NAME']));
unset($real_name);

set_include_path(
	$_SERVER['DOCUMENT_ROOT'] . '/files/Core/database' . PATH_SEPARATOR .
	$_SERVER['DOCUMENT_ROOT'] . '/files/Core/display' . PATH_SEPARATOR .
	$_SERVER['DOCUMENT_ROOT'] . '/files/Core/helpers' . PATH_SEPARATOR .
	get_include_path());
spl_autoload_extensions('.php');
spl_autoload_register();

function _shutdown_handler() {
	if (($error = error_get_last())) {
		if($error['type'] == E_ERROR || $error['type'] == E_RECOVERABLE_ERROR) {
			ob_clean();
			Log::write('');
			header('Location: /error?id=script_fatal&' . urlencode($error['message']));
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

define('CORE', 'files/Core');
define('SETTINGS', CORE . '/settings');
define('GALERIE', 'galerie');
define('HEADER', 'files/Static/Header.inc');
define('FOOTER', 'files/Static/Footer.inc');
define('LOG', 'log/error.log');
define('PHP_LOG', 'log/php.log');

define('DEFAULT_FROM_MAIL', 'TK Olymp.cz <noreply@tkolymp.cz>');

//ini_set('log_errors' , '1');
//ini_set('error_log' , PHP_LOG);
//ini_set('display_errors' , '0');

define('DEBUG', '1');
if(DEBUG) {
	//ini_set('display_errors','On'); 
	error_reporting(-1);
}

include (SETTINGS . '/db.php');
include (SETTINGS . '/sitemap.php');

//-----Ciselne hodnoty urovni uzivatelu-----//
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

public static $sekce = array(
	'admin'		=> array(
			'aktuality'	=> array('Správa článků', L_EDITOR),
			'nastenka'	=> array('Správa nástěnky', L_EDITOR),
			'users'		=> array('Správa uživatelů', L_ADMIN),
			'akce'		=> array('Správa akcí', L_ADMIN),
			'rozpis'	=> array('Správa rozpisů', L_TRENER),
			'nabidka'	=> array('Správa nabídky', L_TRENER),
			'dokumenty'	=> array('Správa dokumentů', L_EDITOR),
			'pary'		=> array('Správa párů', L_ADMIN),
			'ankety'	=> array('Správa anket', L_EDITOR),
			'inzerce'	=> array('Správa inzerce', L_ADMIN),
			'galerie'	=> array('Správa galerie', L_ADMIN)
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
			'profil'	=> 'Profil'
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
}
?>
