<?php
class DBVideoSource extends Database
{
    public static function getAll()
    {
        $res = self::query(
            'SELECT vs_id, vs_url, vs_title, vs_description, vs_created_at, vs_last_checked
            FROM video_source ORDER BY vs_created_at DESC'
        );
        return self::getArray($res);
    }

    public static function getSingle($id)
    {
        $res = self::query(
            "SELECT vs_id, vs_url, vs_title, vs_description, vs_created_at, vs_last_checked
            FROM video_source
            WHERE vs_id='?'",
            $id
        );
        return self::getSingleRow($res);
    }

    public static function add($url)
    {
        self::query(
            "INSERT INTO video_source
             (vs_url, vs_created_at) VALUES ('?', NOW())",
            $url
        );
        return self::getInsertId();
    }

    public static function edit($id, $url, $title, $desc)
    {
        self::query(
            "UPDATE video_source SET vs_url='?', vs_title=?, vs_description=?
             WHERE vs_id='?'",
            $url,
            $title ? "'$title'" : 'null',
            $desc ? "'$desc'" : 'null',
            $id
        );
    }

    public static function remove($id)
    {
        self::query("DELETE FROM video_source WHERE vs_id='?'", $id);
    }
}
