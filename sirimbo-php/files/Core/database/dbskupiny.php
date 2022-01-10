<?php
class DBSkupiny extends Database
{
    public static function addChild($sid, $gid)
    {
        self::query(
            "INSERT INTO platby_group_skupina
            (pgs_id_skupina,pgs_id_group) VALUES ('?','?')
            ON CONFLICT (pgs_id_skupina,pgs_id_group) DO NOTHING",
            $sid,
            $gid,
        );
    }

    public static function removeChild($sid, $gid)
    {
        self::query(
            "DELETE FROM platby_group_skupina
            WHERE pgs_id_group='?' AND pgs_id_skupina='?'",
            $gid,
            $sid,
        );
    }

    public static function getSingleWithGroups($id)
    {
        $res = self::query(
            "SELECT *
            FROM platby_group_skupina
                LEFT JOIN platby_group ON pgs_id_group=pg_id
                LEFT JOIN skupiny ON pgs_id_skupina=s_id
            WHERE s_id='?'
            ORDER BY pg_type,pg_id",
            $id,
        );
        return self::getArray($res);
    }

    public static function getSingleWithCategories($id)
    {
        $res = self::query(
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
        return self::getArray($res);
    }

    public static function get()
    {
        $res = self::query("SELECT * FROM skupiny");
        return self::getArray($res);
    }

    public static function getSingle($id)
    {
        $res = self::query("SELECT * FROM skupiny WHERE s_id='?'", $id);
        return self::getSingleRow($res);
    }

    public static function insert($name, $location, $color, $desc)
    {
        self::query(
            "INSERT INTO skupiny (s_name,s_location,s_color_text,s_color_rgb,s_description)
            VALUES ('?','?','','?','?')",
            $name,
            $location,
            $color,
            $desc,
        );
    }

    public static function update($id, $name, $location, $color, $desc)
    {
        self::query(
            "UPDATE skupiny SET s_name='?',s_location='?',s_color_rgb='?',s_description='?' WHERE s_id='?'",
            $name,
            $location,
            $color,
            $desc,
            $id,
        );
    }

    public static function delete($id)
    {
        self::query("DELETE FROM skupiny WHERE s_id='?'", $id);
        return true;
    }
}
