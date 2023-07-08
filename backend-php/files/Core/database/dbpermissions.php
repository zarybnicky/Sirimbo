<?php
class DBPermissions extends Database
{
    public static $permissionCache = [];

    public static function getGroups()
    {
        return self::queryArray("SELECT * FROM permissions");
    }

    public static function getSingleGroup($id)
    {
        if (isset(self::$permissionCache[$id])) {
            return self::$permissionCache[$id];
        }
        $row = self::querySingle("SELECT * FROM permissions WHERE pe_id='?'", $id);
        self::$permissionCache[$id] = $row;
        return $row;
    }
}
