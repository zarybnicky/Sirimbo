<?php
class DBPlatbyGroup extends Database
{
    public static function getGroups()
    {
        $res = self::query('SELECT * FROM platby_group ORDER BY pg_type,pg_id');
        return self::getArray($res);
    }
    public static function getGroupsWithCategories()
    {
        $res = self::query(
            "SELECT *
            FROM platby_category_group
                LEFT OUTER JOIN platby_group ON pcg_id_group=pg_id
                LEFT OUTER JOIN platby_category ON pcg_id_category=pc_id
            WHERE pc_archive='0'
            ORDER BY pg_type,pg_id,pc_symbol"
        );
        return self::getArray($res);
    }

    public static function getWithoutSkupina()
    {
        $res = self::query(
            'SELECT * FROM platby_group
            WHERE NOT EXISTS (
                SELECT pgs_id FROM platby_group_skupina WHERE pgs_id_group=pg_id
            )'
        );
        return self::getArray($res);
    }

    public static function getWithoutCategory()
    {
        $res = self::query(
            'SELECT * FROM platby_group
            WHERE NOT EXISTS (
                SELECT pcg_id FROM platby_category_group WHERE pcg_id_group=pg_id
            )'
        );
        return self::getArray($res);
    }
}
