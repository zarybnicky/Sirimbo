<?php
class DBPermissions extends Database
{
    public static $permissionCache = [];

    public static function getGroups()
    {
        return self::getArray(self::query("SELECT * FROM permissions"));
    }

    public static function getSingleGroup($id)
    {
        if (isset(self::$permissionCache[$id])) {
            return self::$permissionCache[$id];
        }
        $res = self::query("SELECT * FROM permissions WHERE pe_id='?'", $id);
        $row = self::getSingleRow($res);
        self::$permissionCache[$id] = $row;
        return $row;
    }
}
