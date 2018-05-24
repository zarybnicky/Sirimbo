<?php
function formatTime($str, $forDisplay) {
    if ($forDisplay) {
        return substr($str, 0, 5); //15:00:00
    } else {
        return $str . ':00';
    }
}

function formatDate($str) {
    list($year, $month, $day) = explode('-', $str);
    return (int)$day . '. ' . (int)$month . '. ' . $year;
}

function formatTimestamp($str, $date_only = false) {
    list($date, $time) = explode(' ', $str);
    if ($date_only) {
        return formatDate($date);
    }
    $date = formatDate($date);
    $time = formatTime($time, 1);
    return $date . ' ' . $time;
}

if (!function_exists('getallheaders')) {
    function getallheaders() {
        $headers = [];
        foreach ($_SERVER as $name => $value)
        {
            if (substr($name, 0, 5) == 'HTTP_')
            {
                $headers[str_replace(' ', '-', ucwords(strtolower(str_replace('_', ' ', substr($name, 5)))))] = $value;
            }
        }
        return $headers;
    }
}
