<?php
class DBSkupiny extends Database
{
    public static function addChild($sid, $gid)
    {
        \Database::query(
            "INSERT INTO platby_group_skupina
            (pgs_id_skupina,pgs_id_group) VALUES ('?','?')
            ON CONFLICT (pgs_id_skupina,pgs_id_group) DO NOTHING",
            $sid,
            $gid,
        );
    }

    public static function removeChild($sid, $gid)
    {
        \Database::query(
            "DELETE FROM platby_group_skupina
            WHERE pgs_id_group='?' AND pgs_id_skupina='?'",
            $gid,
            $sid,
        );
    }

    public static function getSingleWithGroups($id)
    {
        return \Database::queryArray(
            "SELECT *
            FROM platby_group_skupina
                LEFT JOIN platby_group ON pgs_id_group=pg_id
                LEFT JOIN skupiny ON pgs_id_skupina=s_id
            WHERE s_id='?'
            ORDER BY pg_type,pg_id",
            $id,
        );
    }

    public static function getSingleWithCategories($id)
    {
        $res = \Database::queryArray(
            "SELECT *
            FROM platby_group_skupina
                LEFT JOIN platby_category_group ON pcg_id_group=pgs_id_group
                LEFT JOIN platby_group ON pgs_id_group=pg_id
                LEFT JOIN skupiny ON pgs_id_skupina=s_id
                LEFT JOIN platby_category ON pcg_id_category=pc_id
            WHERE s_id='?' AND pc_archive!='1'
            ORDER BY pg_type,pg_id,pc_symbol,pc_date_due ASC",
            $id
        );
    }

    public static function get()
    {
        return \Database::queryArray("SELECT * FROM skupiny order by s_id asc");
    }

    public static function getSingle($id)
    {
        return \Database::querySingle("SELECT * FROM skupiny WHERE s_id='?'", $id);
    }
}
