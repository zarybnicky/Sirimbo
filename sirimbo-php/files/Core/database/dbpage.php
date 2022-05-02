<?php
class DBPage extends Database
{
    public static function getSinglePage($url)
    {
        $res = self::query("SELECT * FROM page WHERE url='?'", $url);
        return $res ? self::getSingleRow($res) : false;
    }
}
