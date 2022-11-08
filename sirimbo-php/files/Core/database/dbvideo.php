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
}
