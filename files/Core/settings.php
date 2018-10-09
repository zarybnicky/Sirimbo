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
define('HEADER', 'files' . DIRECTORY_SEPARATOR . 'Static' . DIRECTORY_SEPARATOR . 'Header.inc');
define('FOOTER', 'files' . DIRECTORY_SEPARATOR . 'Static' . DIRECTORY_SEPARATOR . 'Footer.inc');
define('HEADER_TISK', 'files' . DIRECTORY_SEPARATOR . 'Static' . DIRECTORY_SEPARATOR . 'HeaderTisk.inc');
define('FOOTER_TISK', 'files' . DIRECTORY_SEPARATOR . 'Static' . DIRECTORY_SEPARATOR . 'FooterTisk.inc');

define('LOG', ROOT . DIRECTORY_SEPARATOR . 'log' . DIRECTORY_SEPARATOR . 'error.log');
define('DEBUG_LOG', ROOT . DIRECTORY_SEPARATOR . 'log' . DIRECTORY_SEPARATOR . 'debug.log');
define('PHP_LOG', ROOT . DIRECTORY_SEPARATOR . 'log' . DIRECTORY_SEPARATOR . 'php.log');

include ROOT . DIRECTORY_SEPARATOR . 'config.php';

mb_internal_encoding('UTF-8');

function shutdownHandler()
{
    if (($error = error_get_last()) === null) {
        return;
    }
    if ($error['type'] & (E_STRICT | E_DEPRECATED | E_NOTICE)) {
        return;
    }

    ob_end_clean();

    $msg = "{$error['type']}: {$error['message']} in {$error['file']}: {$error['line']}";
    foreach (debug_backtrace() as $k => $v) {
        if ($k <= 0) {
            continue;
        }
        array_walk(
            $v['args'],
            function (&$item) {
                $item = var_export($item, true);
            }
        );
        $msg .= "\n#$k "
             . "{$v['file']}({$v['line']}): "
             . (isset($v['class']) ? $v['class'] . '->' : '')
             . $v['function'] . '(' . implode(', ', $v['args']) . ')';
    }
    Log::write($msg);
    if (strpos('error', $_SERVER['REQUEST_URI']) !== false) {
        Log::write("Recursive error message!");
        die('Fatal error: Rekurzivní smyčka přesměrování!');
    }
    header('Location: /error?id=script_fatal');
}
function errorHandler($severity, $message, $filepath, $line)
{
    if ($severity & (E_STRICT | E_DEPRECATED | E_NOTICE)) {
        return false;
    }
    ob_end_clean();
    $msg = "$severity: $message in $filepath: $line";
    foreach (debug_backtrace() as $k => $v) {
        if ($k <= 0) {
            continue;
        }
        array_walk(
            $v['args'],
            function (&$item) {
                $item = var_export($item, true);
            }
        );
        $msg .= "\n#$k "
              . $v['file'] . '(' . $v['line'] . '): '
              . (isset($v['class']) ? $v['class'] . '->' : '')
              . $v['function'] . '(' . implode(', ', $v['args']) . ')';
    }
    Log::write($msg);
    if (isset($_SERVER['REQUEST_URI']) && strpos('error', $_SERVER['REQUEST_URI']) !== false) {
        Log::write("Recursive error message!");
        die('Fatal error: Rekurzivní smyčka přesměrování!');
    }
    header('Location: /error?id=script_fatal');
    return true;
}

register_shutdown_function('shutdownHandler');
set_error_handler('errorHandler');

define('DEBUG', '1');
if (DEBUG) {
    error_reporting(-1);
}

define('AKTUALITY_CLANKY', 1);
define('AKTUALITY_KRATKE', 3);
define('THUMBNAIL_MAX', 150);

define('P_NONE', 1);
define('P_VIEW', 2);
define('P_MEMBER', 4);
define('P_OWNED', 8);
define('P_ADMIN', 16);

class Settings
{
public static $documentTypes = [
    '1'        => 'Schůze, rady',
    '2'        => 'Soutěže',
    '3'        => 'Tábory',
    '0'        => 'Ostatní'
];

public static $permissionLevels = [
    P_NONE => 'Bez přístupu',
    P_VIEW => 'Zobrazit',
    P_MEMBER => 'Editovat',
    P_OWNED => 'Admin (svoje)',
    P_ADMIN => 'Admin'
];

public static $permissions = [
    'akce' => [
        'name' => "Akce",
        'default' => P_MEMBER,
        P_NONE => 1, P_VIEW => 1, P_MEMBER => 1, P_OWNED => 1, P_ADMIN => 1],
    'aktuality' => [
        'name' => "Aktuality",
        'default' => P_VIEW,
        P_VIEW => 1, P_OWNED => 1, P_ADMIN => 1],
    'dokumenty' => [
        'name' => "Dokumenty",
        'default' => P_MEMBER,
        P_NONE => 1, P_MEMBER => 1, P_OWNED => 1, P_ADMIN => 1],
    'galerie' => [
        'name' => "Fotogalerie",
        'default' => P_VIEW,
        P_VIEW => 1, P_OWNED => 1, P_ADMIN => 1],
    'konzole' => [
        'name' => "Konzole",
        'default' => P_NONE,
        P_NONE => 1, P_ADMIN => 1],
    'nabidka' => [
        'name' => "Nabídka",
        'default' => P_MEMBER,
        P_NONE => 1, P_VIEW => 1, P_MEMBER => 1, P_OWNED => 1, P_ADMIN => 1],
    'nastenka' => [
        'name' => "Nástěnka",
        'default' => P_VIEW,
        P_NONE => 1, P_VIEW => 1, P_OWNED => 1, P_ADMIN => 1],
    'novinky' => [
        'name' => "Novinky",
        'default' => P_VIEW,
        P_NONE => 1, P_VIEW => 1, P_OWNED => 1, P_ADMIN => 1],
    'pary' => [
        'name' => "Páry",
        'default' => P_VIEW,
        P_NONE => 1, P_VIEW => 1, P_ADMIN => 1],
    'platby' => [
        'name' => "Platby",
        'default' => P_NONE,
        P_NONE => 1, P_ADMIN => 1],
    'permissions' => [
        'name' => "Oprávnění",
        'default' => P_NONE,
        P_NONE => 1, P_ADMIN => 1],
    'rozpis' => [
        'name' => "Rozpis",
        'default' => P_MEMBER,
        P_NONE => 1, P_VIEW => 1, P_MEMBER => 1, P_OWNED => 1, P_ADMIN => 1],
    'skupiny' => [
        'name' => "Skupiny",
        'default' => P_VIEW,
        P_NONE => 1, P_VIEW => 1, P_ADMIN => 1],
    'users' => [
        'name' => "Uživatelé",
        'default' => P_VIEW,
        P_NONE => 1, P_VIEW => 1, P_OWNED => 1, P_ADMIN => 1],
    'main' => [
        'name' => "Veřejná část",
        'default' => P_VIEW,
        P_VIEW => 1]
    ];
}
