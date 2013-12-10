<?php
namespace TKOlomouc\Utility;

class Miscellaneous
{
    public static function notice($text, $return = false)
    {
        if (!$text) {
            return;
        }
        if ($return) {
            return '<div class="notice">' . $text . '</div>' . "\n";
        }
        echo '<div class="notice">', $text, '</div>', "\n";
    }

    public static function getColorBox($color, $popis)
    {
        return '<div class="box" title="' . $popis . '" '
            . 'style="background-color:' . $color . '"></div>';
    }

    public static function formatTime($str, $forDisplay)
    {
        if ($forDisplay) {
            return substr($str, 0, 5); //15:00:00
        } else {
            return $str . ':00';
        }
    }

    public static function formatDate($str)
    {
        list($year, $month, $day) = explode('-', $str);
        return (int) $day . '. ' . (int) $month . '. ' . $year;
    }

    public static function formatTimestamp($str, $date_only = false)
    {
        list($date, $time) = explode(' ', $str);
        if ($date_only) {
            return self::formatDate($date);
        }
        $date = self::formatDate($date);
        $time = self::formatTime($time, 1);
        return implode(' ', array($date, $time));
    }

    public static function timeSubstract($first, $sec)
    {
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

    public static function timeAdd($first, $sec)
    {
        list($f_hrs, $f_min) = explode(':', $first);
        list($s_hrs, $s_min) = explode(':', $sec);

        $m = $f_min + $s_min;
        $h = floor($m / 60) + $f_hrs + $s_hrs;

        return ($h . ':' . ($m % 60));
    }

    public static function getCheckbox($name, $value = '', $default = false, $get = false, $readonly = false)
    {
        if ($value === '') {
            $value = $name;
        }
        $checked = (($get == true) ?
            ((self::get($name) != false) ? true : false) :
            ((self::post($name) != false) ? true : false));
        if (!$checked) {
            $checked = (bool) $default;
        }
        return '<input type="checkbox" name="' . $name . '" value="' . $value . '"'
            . ($checked ? ' checked="checked"' : '') . ($readonly ? ' readonly="readonly"' : '') . '/>';
    }

    public static function getRadio($name, $value, $default = false, $get = false, $readonly = false)
    {
        $checked = (($get == true) ?
            ((self::get($name) == $value) ? true : false) :
            ((self::post($name) == $value) ? true : false));
        if (!$checked) {
            $checked = (bool) $default;
        }
        return '<input type="radio" name="' . $name . '" value="' . $value . '"'
            . ($checked ? ' checked="checked"' : '') . ($readonly ? ' readonly="readonly"' : '') . '/>';
    }

    public static function getReturnURI($default)
    {
        return self::post('referer') ? self::post('referer') : $default;
    }

    public static function getReturnInput()
    {
        return '<input type="hidden" name="referer" value="' . Request::getReferer() . '" />' . "\n";
    }

    public static function getIP()
    {
        if (!empty($_SERVER['HTTP_CLIENT_IP'])) {
            return $_SERVER['HTTP_CLIENT_IP'];
        } elseif (!empty($_SERVER['HTTP_X_FORWARDED_FOR'])) {
            return $_SERVER['HTTP_X_FORWARDED_FOR'];
        } else {
            return $_SERVER['REMOTE_ADDR'];
        }
    }
}
