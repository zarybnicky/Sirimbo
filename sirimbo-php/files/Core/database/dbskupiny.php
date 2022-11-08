<?php
class DBSkupiny extends Database
{
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

    public static function getSingle($id)
    {
        $res = self::query("SELECT * FROM skupiny WHERE s_id='?'", $id);
        return self::getSingleRow($res);
    }
}
