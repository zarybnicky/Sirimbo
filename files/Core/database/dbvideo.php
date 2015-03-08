<?php
class DBVideo extends Database
{
    public static function getAll()
    {
        $res = self::query(
            'SELECT v_id, v_name, v_date, v_text, v_playlist, v_uri
            FROM video ORDER BY v_timestamp'
        );
        return self::getArray($res);
    }

    public static function getSingle($id)
    {
        list($id) = self::escape($id);

        $res = self::query(
            "SELECT v_id, v_name, v_date, v_text, v_playlist, v_uri
            FROM video
            WHERE v_id='$id'"
        );

        return self::getSingleRow($res);
    }

    public static function add($name, $text, $uri, $playlist)
    {
        list($name, $text, $uri, $playlist)
            = self::escape($name, $text, $uri, $playlist);
        $playlist = $playlist ? '1' : '0';

        self::query(
            "INSERT INTO video
             (v_name, v_text, v_uri, v_playlist, v_date)
             VALUES
             ('$name', '$text', '$uri', '$playlist', NOW())"
        );

        return self::getInsertId();
    }

    public static function edit($id, $name, $text, $uri, $playlist)
    {
        list($id, $name, $text, $uri, $playlist)
            = self::escape($id, $name, $text, $uri, $playlist);
        $playlist = $playlist ? '1' : '0';

        self::query(
            "UPDATE video SET v_name='$name', v_text='$text', v_uri='$uri',
                    v_playlist='$playlist'
             WHERE v_id='$id'"
        );
    }

    public static function remove($id)
    {
        list($id) = self::escape($id);
        self::query("DELETE FROM video WHERE v_id='$id'");
    }
}