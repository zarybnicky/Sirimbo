<?php
class DBPlatbyGroup extends Database
{
    public static function addChild($gid, $cid)
    {
        \Database::query(
            "INSERT INTO platby_category_group
            (pcg_id_group,pcg_id_category) VALUES ('?','?')
            ON CONFLICT (pcg_id_group,pcg_id_category) DO NOTHING",
            $gid,
            $cid,
        );
    }

    public static function removeChild($gid, $cid)
    {
        \Database::query(
            "DELETE FROM platby_category_group WHERE pcg_id_group='?' AND pcg_id_category='?'",
            $gid,
            $cid
        );
    }

    public static function getGroups()
    {
        return \Database::queryArray('SELECT * FROM platby_group ORDER BY pg_type,pg_id');
    }

    public static function getGroupsWithCategories()
    {
        return \Database::queryArray(
            "SELECT *
            FROM platby_category_group
                LEFT OUTER JOIN platby_group ON pcg_id_group=pg_id
                LEFT OUTER JOIN platby_category ON pcg_id_category=pc_id
            WHERE pc_archive='0'
            ORDER BY pg_type,pg_id,pc_symbol"
        );
    }

    public static function getSingle($id)
    {
        return \Database::querySingle("SELECT * FROM platby_group WHERE pg_id='?'", $id);
    }

    public static function getSingleWithCategories($id)
    {
        return \Database::queryArray(
            "SELECT *
            FROM platby_category_group
                LEFT JOIN platby_group ON pcg_id_group=pg_id
                LEFT JOIN platby_category ON pcg_id_category=pc_id
            WHERE pg_id='?'
            ORDER BY pg_type,pg_id,pc_symbol",
            $id,
        );
    }

    public static function getSingleWithSkupiny($id)
    {
        return \Database::queryArray(
            "SELECT *
            FROM platby_group_skupina
                LEFT JOIN platby_group ON pgs_id_group=pg_id
                LEFT JOIN skupiny ON pgs_id_skupina=s_id
            WHERE pg_id='?'
            ORDER BY pg_type,pg_id",
            $id,
        );
    }

    public static function getWithoutSkupina()
    {
        return \Database::queryArray(
            'SELECT * FROM platby_group
            WHERE NOT EXISTS (SELECT pgs_id FROM platby_group_skupina WHERE pgs_id_group=pg_id)'
        );
    }

    public static function getWithoutCategory()
    {
        return \Database::queryArray(
            'SELECT * FROM platby_group
            WHERE NOT EXISTS (SELECT pcg_id FROM platby_category_group WHERE pcg_id_group=pg_id)'
        );
    }
}
