<?php
function post($field = null, $value = null) {
    throw new BadFunctionCallException(
        "post() called (field: '$field', value: '$value')"
    );
}
function get($field = null, $value = null) {
    throw new BadFunctionCallException(
        "get() called (field: '$field', value: '$value')"
    );
}
function session($field = null, $value = null) {
    throw new BadFunctionCallException(
        "session() called (field: '$field', value: '$value')"
    );
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
    if ($date_only) {
        return formatDate($date);
    }
    $date = formatDate($date);
    $time = formatTime($time, 1);
    return $date . ' ' . $time;
}
