<?php
function notice($text, $return = false) {
    if (!$text)
        return;

    if (!$return) {
        echo '<div class="notice">', $text, '</div>', "\n";
    } else {
        return '<div class="notice">' . $text . '</div>' . "\n";
    }
}
function getColorBox($color, $popis) {
    return '<div class="box" title="' . $popis . '" ' .
        'style="background-color:' . $color . '"></div>';
}

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
function timeSubstract($first, $sec) {
    if (strcmp($first, $sec) > 0) {
        $tmp = $first;
        $first = $sec;
        $sec = $tmp;
    }

    list($f_hrs, $f_min) = explode(':', $first);
    list($s_hrs, $s_min) = explode(':', $sec);

    $m_diff = $f_min - $s_min;
    $h_diff = $f_hrs - $s_hrs;

    $r = abs($h_diff * 60 + $m_diff);

    return (floor($r / 60) . ':' . ($r % 60));
}
function timeAdd($first, $sec) {
    list($f_hrs, $f_min) = explode(':', $first);
    list($s_hrs, $s_min) = explode(':', $sec);

    $m = $f_min + $s_min;
    $h = floor($m / 60) + $f_hrs + $s_hrs;

    return ($h . ':' . ($m % 60));
}
function echoTaborDokumenty($list_name, $kats) {
    echo '<select name="', $list_name, '">', "\n";
    echo '<option value="none"';
    if (!post($list_name))
        echo ' selected="selected"';
    echo '>Vyber si :o)</option>', "\n";
    if (is_array($kats)) {
        foreach ($kats as $kat) {
            $doku = DBDokumenty::getDokumentyByKategorie($kat);

            foreach ($doku as $item) {
                echo '<option value="', $item['d_id'], '"';
                if (post($list_name) == $item['d_id'])
                    echo ' selected="selected"';
                echo '>', $item['d_name'], ' (', $item['d_kategorie'], ')';
                echo '</option>', "\n";
            }
        }
    } else {
        $doku = DBDokumenty::getDokumentyByKategorie($kats);

        foreach ($doku as $item) {
            echo '<option value="', $item['d_id'], '"';
            if (post($list_name) == $item['d_id'])
                echo ' selected="selected"';
            echo '>', $item['d_name'], ' (', Settings::$documentTypes[$item['d_kategorie']], ')';
            echo '</option>', "\n";
        }
    }
    echo '</select>', "\n";
}

function getReturnURI($default) {
    return post('referer') ? post('referer') : $default;
}

function getReturnInput() {
    return '<input type="hidden" name="referer" value="' . Request::getReferer() . '" />' . "\n";
}

function echoFullJmeno($userData) {
    if (is_array($userData)) {
        echo $userData['u_jmeno'], ' ',  $userData['u_prijmeni'];
    }
}

function getIP() {
    if (!empty($_SERVER['HTTP_CLIENT_IP'])) {
        return $_SERVER['HTTP_CLIENT_IP'];
    } elseif (!empty($_SERVER['HTTP_X_FORWARDED_FOR'])) {
        return $_SERVER['HTTP_X_FORWARDED_FOR'];
    } else {
        return $_SERVER['REMOTE_ADDR'];
    }
}
