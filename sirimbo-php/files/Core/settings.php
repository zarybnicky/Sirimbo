<?php
define('ROOT', $_SERVER['DOCUMENT_ROOT']);
define('FILES', ROOT . DIRECTORY_SEPARATOR . 'files');
define('CORE', FILES . DIRECTORY_SEPARATOR . 'Core');
define('SETTINGS', CORE . DIRECTORY_SEPARATOR . 'settings');
define('ERROR', FILES . DIRECTORY_SEPARATOR . 'Error');
define('TEMPLATE', 'files' . DIRECTORY_SEPARATOR . 'Template.inc');

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
    fwrite(STDERR, $_SERVER['REQUEST_URI'] . ": \n" . $msg);
    if (strpos('error', $_SERVER['REQUEST_URI']) !== false) {
        fwrite(STDERR, "Recursive error!");
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
            function (&$item, $key) {
                $item = var_export($item, true);
            }
        );
        $msg .= "\n#$k "
              . $v['file'] . '(' . $v['line'] . '): '
              . (isset($v['class']) ? $v['class'] . '->' : '')
              . $v['function'] . '(' . implode(', ', $v['args']) . ')';
    }
    fwrite(STDERR, $_SERVER['REQUEST_URI'] . ": \n" . $msg);
    if (isset($_SERVER['REQUEST_URI']) && strpos('error', $_SERVER['REQUEST_URI']) !== false) {
        fwrite(STDERR, "Recursive error!");
        die('Fatal error: Rekurzivní smyčka přesměrování!');
    }
    header('Location: /error?id=script_fatal');
    return true;
}

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
            'name' => "Články",
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

    public static $imageType = [
        'image/pjpeg' => 'jpg',
        'image/jpeg' => 'jpg',
        'image/gif' => 'gif',
        'image/x-png' => 'png'
    ];

    public static $imageSuffix = [
        'image/pjpeg' => 'JPEG',
        'image/jpeg' => 'JPEG',
        'image/gif' => 'GIF',
        'image/x-png' => 'PNG'
    ];
}