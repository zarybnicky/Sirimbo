<?php
class DBParameters extends Database
{
    public static function set($name, $value)
    {
        self::query(
            "INSERT INTO parameters (pa_name, pa_value) VALUES ('?, '?) ON CONFLICT (pa_name) DO UPDATE SET pa_value=EXCLUDED.pa_value",
            $name, $value
        );
        return true;
    }

    public static function get($name)
    {
        $res = self::query("SELECT pa_value FROM parameters where pa_name='?'", $name);
        $res = self::getSingleRow($res);
        return $res ? $res['pa_value'] : $res;
    }
}
