<?php
class DBPermissions extends Database
{
    public static $permissionCache = [];

    public static function getGroups()
    {
        $res = self::query("SELECT * FROM permissions");
        return self::getArray($res);
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
