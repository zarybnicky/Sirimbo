<?php
class DBVideo extends Database
{
    public static function getAll()
    {
        $res = self::query(
            'SELECT v_id, v_name, v_date, v_text, v_playlist, v_uri
            FROM video ORDER BY v_date DESC, v_timestamp DESC'
        );
        return self::getArray($res);
    }

    public static function getSingle($id)
    {
        $res = self::query(
            "SELECT v_id, v_name, v_date, v_text, v_playlist, v_uri
            FROM video
            WHERE v_id='?'",
            $id
        );
        return self::getSingleRow($res);
    }

    public static function getYtId($uri)
    {
        $split = preg_split("/(vi\/|v=|\/v\/|youtu\.be\/|\/embed\/)/", $uri);
        $query = isset($split[1]) ? $split[1] : $uri;
        $query = preg_split("/[^0-9a-z_\-]/i", $query);
        return $query[0];
    }

    public static function add($name, $text, $uri, $playlist)
    {
        self::query(
            "INSERT INTO video
             (v_name, v_text, v_uri, v_playlist, v_date)
             VALUES
             ('?', '?', '?', '?', NOW())",
            $name, $text, self::getYtId($uri), $playlist ? '1' : '0'
        );
        return self::getInsertId();
    }

    public static function edit($id, $name, $text, $uri, $playlist)
    {
        self::query(
            "UPDATE video SET v_name='?', v_text='?', v_uri='?', v_playlist='?'
             WHERE v_id='?'",
            $name, $text, self::getYtId($uri), $playlist ? '1' : '0', $id
        );
    }

    public static function remove($id)
    {
        self::query("DELETE FROM video WHERE v_id='?'", $id);
    }
}