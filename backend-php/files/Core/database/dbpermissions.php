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

    public static function addGroup($name, $description, $permissions)
    {
        $count = count($permissions);
        $keys = array_keys($permissions);
        $values = array_values($permissions);
        $result = self::escapeArray(array_merge([$name, $description], $keys, $values));
        $name = $result[0];
        $description = $result[1];
        for ($i = 0; $i < $count; $i++) {
            $permissions[$result[$i + 2]] = $result[$i + $count + 2];
        }

        $q = "INSERT INTO permissions (pe_name,pe_description";
        foreach (array_keys($permissions) as $key) {
            $q .= ',pe_' . $key;
        }
        $q .= ") VALUES ('$name','$description'";
        foreach ($permissions as $value) {
            $q .= ",'$value'";
        }
        $q .= ')';

        self::query($q);
    }

    public static function editGroup($id, $name, $description, $permissions)
    {
        $count = count($permissions);
        $values = array_values($permissions);
        $keys = array_keys($permissions);
        $result = self::escapeArray(array_merge([$id, $name, $description], $keys, $values));
        $id = $result[0];
        $name = $result[1];
        $description = $result[2];
        for ($i = 0; $i < $count; $i++) {
            $permissions[$result[$i + 3]] = $result[$i + $count + 3];
        }

        $q = "UPDATE permissions SET pe_name='$name',pe_description='$description'";
        foreach ($permissions as $key => $value) {
            $q .= ",pe_$key='$value'";
        }
        $q .= " WHERE pe_id='$id'";

        self::query($q);
        return true;
    }

    public static function removeGroup($id)
    {
        self::query("DELETE FROM permissions WHERE pe_id='?'", $id);
        return true;
    }
}