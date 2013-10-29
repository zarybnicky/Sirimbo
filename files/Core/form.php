<?php
class Form
{
    private $_valid;
    private $_messages;
    private $_fields;

    const REGEXP_DATE = '/^((?:19|20)\d\d)-(0[1-9]|1[012])-(0[1-9]|[12][0-9]|3[01])$/i';
    const REGEXP_EMAIL = '/^\b[A-Z0-9._%-]+@[A-Z0-9.-]+\.[A-Z]{2,4}\b$/i';
    const REGEXP_PHONE = '/^((\+|00)\d{3})?( ?\d{3}){3}$/';
    const REGEXP_LOGIN = '/^[A-Z0-9_]{3,20}$/i';
    const REGEXP_PASSWORD = '/^[A-Z0-9_]{6,32}$/i';

    function __construct() {
        $this->_valid = true;
        $this->_messages = array();
        $this->_fields = array();
    }
    function isValid() {
        return $this->_valid;
    }
    function getMessages() {
        return $this->_messages;
    }
    function getFields() {
        return $this->_fields;
    }
    function checkDate($i, $message, $name = '') {
        if (preg_match(Form::REGEXP_DATE, $i)) {
            list($year, $month, $day) = explode('-', $i);
            if (
                !($day == 31 && ($month == 4 || $month == 6 || $month == 9 || $month == 11)
                || $day >= 30 && $month == 2
                || $month == 2 && $day == 29 && !($year % 4 == 0 && ($year % 100 != 0 || $year % 400 == 0))
            )) {
                return true;
            }
        }
        $this->_error($message, $name);
        return false;
    }
    function checkEmail($i, $message, $name = '') {
        return $this->checkRegexp($i, Form::REGEXP_EMAIL, $message, $name);
    }
    function checkPhone($i, $message, $name = '') {
        return $this->checkRegexp($i, Form::REGEXP_PHONE, $message, $name);
    }
    function checkLogin($i, $message, $name = '') {
        return $this->checkRegexp($i, Form::REGEXP_LOGIN, $message, $name);
    }
    function checkPassword($i, $message, $name = '') {
        return $this->checkRegexp($i, Form::REGEXP_PASSWORD, $message, $name);
    }
    function checkRegexp($i, $regexp, $message, $name = '') {
        if (preg_match($regexp, $i))
            return true;

        $this->_error($message, $name);
        return false;
    }
    function checkLength($i, $min, $max, $message, $name = '') {
        $len = strlen($i);
        if ($len >= $min && $len <= $max)
            return true;

        $this->_error($message, $name);
        return false;
    }
    function checkInArray($i, $array, $message, $name = '') {
        if (in_array($i, $array))
            return true;

        $this->_error($message, $name);
        return false;
    }
    function checkArrayKey($i, $array, $message, $name = '') {
        if (array_key_exists($i, $array))
            return true;

        $this->_error($message, $name);
        return false;
    }
    function checkNotEmpty($i, $message, $name = '') {
        if ($i || $i === 0)
            return true;

        $this->_error($message, $name);
        return false;
    }
    function checkNumeric($i, $message, $name = '') {
        if (is_numeric($i))
            return true;

        $this->_error($message, $name);
        return false;
    }
    function checkNumberBetween($i, $min, $max, $message, $name) {
        if (is_numeric($i) && $i >= $min && $i <= $max)
            return true;

        $this->_error($message, $name);
        return false;
    }
    function checkBool($i, $message, $name) {
        if ((bool) $i)
            return true;

        $this->_error($message, $name);
        return false;
    }
    private function _error($message, $name = '') {
        $this->_valid = false;
        $this->_messages[] = $message;
        if ($name !== '')
            $this->_fields[] = $name;
    }
}

function notice($text, $return = false) {
    if (!$text) return;

    if (!$return)echo '<div class="notice">', $text, '</div>', "\n";
    else        return '<div class="notice">' . $text . '</div>' . "\n";
}
function getColorBox($color, $popis) {
    return '<div class="box" title="' . $popis . '" ' .
        'style="background-color:' . $color . '"></div>';
}
function post($field = null, $value = null) {
    if ($field === null) {
        return $_POST;
    }

    if ($value !== null) {
        $_POST[$field] = $value;
        return;
    }

    if (isset($_POST[$field]))
        return $_POST[$field];
    else
        return null;
}
function get($field = null, $value = null) {
    if ($field === null) {
        return $_GET;
    }

    if ($value !== null) {
        $_GET[$field] = $value;
        return;
    }

    if (isset($_GET[$field]))
        return $_GET[$field];
    else
        return null;
}
function session($field = null, $value = null) {
    if ($field === null) {
        return $_SESSION;
    }

    if ($value !== null) {
        $_SESSION[$field] = $value;
        return;
    }

    if (isset($_SESSION[$field]))
        return $_SESSION[$field];
    else
        return null;
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
function getCheckbox($name, $value = '', $default = false, $get = false, $readonly = false) {
    if ($value === '')
        $value = $name;
    $checked = (($get == true) ?
        ((get($name) != false) ? true : false) :
        ((post($name) != false) ? true : false));
    if (!$checked)
        $checked = (bool) $default;
    return '<input type="checkbox" name="' . $name . '" value="' . $value . '"' .
        ($checked ? ' checked="checked"' : '') . ($readonly ? ' readonly="readonly"' : '') . '/>';
}
function getRadio($name, $value, $default = false, $get = false, $readonly = false) {
    $checked = (($get == true) ?
        ((get($name) == $value) ? true : false) :
        ((post($name) == $value) ? true : false));
    if (($get == true) ? !get($name) : !post($name))
        $checked = (bool) $default;
    return '<input type="radio" name="' . $name . '" value="' . $value . '"' .
        ($checked ? ' checked="checked"' : '') . ($readonly ? ' readonly="readonly"' : '') . '/>';
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
?>