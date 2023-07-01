<?php
function errorHandler($severity, $message, $filepath, $line)
{
    if ($severity & (E_STRICT | E_DEPRECATED | E_NOTICE)) {
        return false;
    }
    if (ob_get_level() > 0) {
        ob_end_clean();
    }
    $msg = "$severity: $message in $filepath: $line";
    foreach (debug_backtrace() as $k => $v) {
        if ($k <= 0) {
            continue;
        }
        $msg .= "\n#$k "
            . ($v['file'] ?? 'unknown') . '(' . ($v['line'] ?? 'unknown') . '): '
            . (isset($v['class']) ? $v['class'] . '->' : '') . $v['function'];
    }
    syslog(LOG_ERR, $_SERVER['REQUEST_URI'] . ": \n$msg");
    if (isset($_SERVER['REQUEST_URI']) && strpos('error', $_SERVER['REQUEST_URI']) !== false) {
        syslog(LOG_CRIT, "Recursive error!");
        die('Fatal error: Rekurzivní smyčka přesměrování!');
    }
    header('Location: /error?id=script_fatal');
    return true;
}
function shutdownHandler()
{
    if (!$error = error_get_last()) {
        return;
    }
    return errorHandler($error['type'], $error['message'], $error['file'], $error['line']);
}

define('P_NONE', 1);
define('P_VIEW', 2);
define('P_MEMBER', 4);
define('P_OWNED', 8);
define('P_ADMIN', 16);

class Settings
{
    public static $imageType = [
        'image/pjpeg' => 'jpg',
        'image/jpeg' => 'jpg',
        'image/gif' => 'gif',
        'image/x-png' => 'png',
        'image/png' => 'png',
    ];

    public static $imageSuffix = [
        'image/pjpeg' => 'JPEG',
        'image/jpeg' => 'JPEG',
        'image/gif' => 'GIF',
        'image/x-png' => 'PNG',
        'image/png' => 'PNG',
    ];
}
