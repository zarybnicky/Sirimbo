<?php
class DBVideoList extends Database
{
    public static function getAll()
    {
        $res = self::query(
            'SELECT vl_id, vl_url, vl_title, vl_description, vl_count, vl_created_at, vl_last_checked
            FROM video_list ORDER BY vl_created_at DESC'
        );
        return self::getArray($res);
    }

    public static function getSingle($id)
    {
        $res = self::query(
            "SELECT vl_id, vl_url, vl_title, vl_description, vl_count, vl_created_at, vl_last_checked
            FROM video_list
            WHERE vl_id='?'",
            $id
        );
        return self::getSingleRow($res);
    }
}
