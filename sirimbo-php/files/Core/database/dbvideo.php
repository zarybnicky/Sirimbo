<?php
class DBVideo extends Database
{
    public static function getAll()
    {
        $res = self::query('SELECT * FROM video ORDER BY v_created_at DESC');
        return self::getArray($res);
    }

    public static function getOrphan()
    {
        $res = self::query("SELECT * FROM video WHERE v_playlist IS NULL OR v_playlist='' ORDER BY v_created_at DESC");
        return self::getArray($res);
    }

    public static function getByPlaylist($id)
    {
        $res = self::query(
            "SELECT video.* FROM video LEFT JOIN video_list ON vl_url=v_playlist
            WHERE vl_id='?'
            ORDER BY v_created_at DESC",
            $id
        );
        return self::getArray($res);
    }

    public static function getSingle($id)
    {
        $res = self::query("SELECT * FROM video WHERE v_id='?'", $id);
        return self::getSingleRow($res);
    }

    public static function getYtId($uri)
    {
        $split = preg_split("/(vi\/|v=|\/v\/|youtu\.be\/|\/embed\/)/", $uri);
        $query = ($split && isset($split[1])) ? $split[1] : $uri;
        $query = preg_split("/[^0-9a-z_\-]/i", $query);
        return $query[0];
    }

    public static function add($uri, $title, $author, $desc, $playlist)
    {
        self::query(
            "INSERT INTO video
             (v_uri, v_title, v_author, v_description, v_playlist, v_created_at)
             VALUES
             ('?', '?', '?', '?', ?, NOW())",
            self::getYtId($uri), $title, $author, $desc, $playlist ? "'$playlist'" : 'NULL'
        );
        return self::getInsertId();
    }

    public static function edit($id, $uri, $title, $author, $desc, $playlist)
    {
        self::query(
            "UPDATE video SET v_uri='?', v_title='?', v_author='?', v_description='?', v_playlist=?
             WHERE v_id='?'",
            $uri, $title, $author, $desc, $playlist ? "'$playlist'" : 'NULL', $id
        );
    }

    public static function remove($id)
    {
        self::query("DELETE FROM video WHERE v_id='?'", $id);
    }
}
