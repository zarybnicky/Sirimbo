<?php
function phpGlobal(&$array, $field, $value) {
    if ($field === null) {
        return $array;
    }

    if ($value !== null) {
        $array[$field] = $value;
        return;
    }

    if (isset($array[$field])) {
        return $array[$field];
    } else {
        return null;
    }
}

function post($field = null, $value = null) {
    return phpGlobal($_POST, $field, $value);
}

function get($field = null, $value = null) {
    return phpGlobal($_GET, $field, $value);
}

function session($field = null, $value = null) {
    return phpGlobal($_SESSION, $field, $value);
}

function server($field = null, $value = null) {
    return phpGlobal($_SERVER, $field, $value);
}

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
    if ($date_only)
        return formatDate($date);
    $date = formatDate($date);
    $time = formatTime($time, 1);
    return implode(' ', array($date, $time));
}

function getReturnURI($default) {
    return post('referer') ? post('referer') : $default;
}

function getReturnInput() {
    return '<input type="hidden" name="referer" value="' . Request::getReferer() . '" />' . "\n";
}
