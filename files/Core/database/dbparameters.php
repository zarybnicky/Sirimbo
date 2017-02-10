<?php
class DBParameters extends Database
{
    public static function set($name, $value)
    {
        self::query(
            "INSERT INTO parameters (pa_name, pa_value) VALUES
            ('?', '?') ON DUPLICATE KEY UPDATE pa_value='?'",
            $name, $value, $value
        );
        return true;
    }

    public static function get($name)
    {
        $res = self::query("SELECT pa_value FROM parameters where pa_name='?'", $name);
        return self::getSingleRow($res)['pa_value'];
    }
}
