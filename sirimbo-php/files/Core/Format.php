<?php
class Format
{
    public static function time($str)
    {
        return substr($str, 0, 5); //15:00:00
    }

    public static function date($str)
    {
        list($year, $month, $day) = explode('-', $str);
        return (int)$day . '. ' . (int)$month . '. ' . $year;
    }

    public static function timestamp($str, $date_only = false)
    {
        list($date, $time) = explode(' ', $str);
        if ($date_only) {
            return self::date($date);
        }
        $date = self::date($date);
        $time = self::time($time);
        return $date . ' ' . $time;
    }

    public static function range($from, $to)
    {
        $from = new \Date($from);
        $to = new \Date($to);
        return $from->getHumanDate() . ' - '  . $to->getHumanDate();
    }
}
