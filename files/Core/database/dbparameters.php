<?php
class DBParameters extends Database
{
    public static function set($name, $value)
    {
        self::query(
            "INSERT INTO parameters (pa_name, pa_value) VALUES "
            . "('$name', '$value') ON DUPLICATE KEY UPDATE pa_value='$value'"
        );
        return true;
    }

    public static function get($name)
    {
        $res = self::query("SELECT * FROM parameters where pa_name='$name'");
        return self::getSingleRow($res);
    }
}
