<?php
$root = implode(DIRECTORY_SEPARATOR, array_slice(explode(DIRECTORY_SEPARATOR, __DIR__), 0, -2));

define('ROOT', $_SERVER['DOCUMENT_ROOT']);
define('FILES', ROOT . DIRECTORY_SEPARATOR . 'files');
define('PUBLIC_DIR', ROOT . DIRECTORY_SEPARATOR . 'public');
define('GALERIE', PUBLIC_DIR . DIRECTORY_SEPARATOR . 'galerie');
define('GALERIE_THUMBS', GALERIE . DIRECTORY_SEPARATOR . 'thumbnails');
define('CORE', FILES . DIRECTORY_SEPARATOR . 'Core');
define('SETTINGS', CORE . DIRECTORY_SEPARATOR . 'settings');
define('ERROR', FILES . DIRECTORY_SEPARATOR . 'Error');
define('HEADER', FILES . DIRECTORY_SEPARATOR . 'Static' . DIRECTORY_SEPARATOR . 'Header.inc');
define('FOOTER', FILES . DIRECTORY_SEPARATOR . 'Static' . DIRECTORY_SEPARATOR . 'Footer.inc');
define('HEADER_TISK', FILES . DIRECTORY_SEPARATOR . 'Static' . DIRECTORY_SEPARATOR . 'HeaderTisk.inc');
define('FOOTER_TISK', FILES . DIRECTORY_SEPARATOR . 'Static' . DIRECTORY_SEPARATOR . 'FooterTisk.inc');

define('LOG', ROOT . DIRECTORY_SEPARATOR . 'log' . DIRECTORY_SEPARATOR . 'error.log');
define('DEBUG_LOG', ROOT . DIRECTORY_SEPARATOR . 'log' . DIRECTORY_SEPARATOR . 'debug.log');
define('PHP_LOG', ROOT . 'log' . DIRECTORY_SEPARATOR . 'php.log');

mb_internal_encoding('UTF-8');

function shutdownHandler() {
    if (($error = error_get_last()) === null)
        return;
    if ($error['type'] & (E_STRICT | E_DEPRECATED | E_NOTICE)) {
        return;
    }

    ob_end_clean();
    Log::write("{$error['type']}: {$error['message']} in {$error['file']}: {$error['line']}");
    if (Request::getURI() == 'error') {
        Log::write("Recursive error message!");
        die('Fatal error: Rekurzivní smyčka přesměrování!');
    }
    header('Location: /error?id=script_fatal');
}
function errorHandler($severity, $message, $filepath, $line) {
    if ($severity & (E_STRICT | E_DEPRECATED | E_NOTICE)) {
        return false;
    }
    ob_end_clean();
    Log::write("$severity: $message in $filepath: $line");
    if (Request::getURI() == 'error') {
        Log::write("Recursive error message!");
        die('Fatal error: Rekurzivní smyčka přesměrování!');
    }
    header('Location: /error?id=script_fatal');
    return true;
}

register_shutdown_function('shutdownHandler');
set_error_handler('errorHandler');

date_default_timezone_set('Europe/Paris');

define('DEFAULT_FROM_MAIL', 'TK Olymp.cz <noreply@tkolymp.cz>');
define('DEFAULT_ADMIN_MAIL', 'tkolymp@tkolymp.cz');

//ini_set('log_errors' , '1');
//ini_set('error_log' , PHP_LOG);
//ini_set('display_errors' , '0');

define('DEBUG', '1');
if (DEBUG) {
    //ini_set('display_errors','On');
    error_reporting(-1);
}

include SETTINGS . DIRECTORY_SEPARATOR . 'db.php';

define('NABOR', '0');

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

define('NOVINKY_COUNT', 10);
define('AKTUALITY_CLANKY', 1);
define('AKTUALITY_VIDEA', 2);
define('AKTUALITY_KRATKE', 3);
define('THUMBNAIL_MAX', 150);

//-----Ciselne hodnoty urovni uzivatelu-----//
define('L_ALL', '-1');
define('L_UNCONFIRMED', '0');

define('P_NONE', 1);
define('P_VIEW', 2);
define('P_MEMBER', 4);
define('P_OWNED', 8);
define('P_ADMIN', 16);

class Settings
{
public static $documentTypes = array(
    '1'        => 'Schůze, rady',
    '2'        => 'Soutěže',
    '3'        => 'Tábory',
    '0'        => 'Ostatní'
);

public static $permissionLevels = array(
    P_NONE => 'Bez přístupu',
    P_VIEW => 'Zobrazit',
    P_MEMBER => 'Editovat',
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
        P_NONE => 1, P_VIEW => 1, P_ADMIN => 1    ),
    'main' => array(
        'name' => "Veřejná část",
        'default' => P_VIEW,
        P_VIEW => 1)
);
}
