<?php
class DBPage extends Database
{
    public static function getAll()
    {
        $res = self::query(
            'SELECT p_id, p_type, p_url, p_title, p_html, p_json, p_parameters, p_created_at, p_updated_at
            FROM page ORDER BY p_created_at DESC'
        );
        return self::getArray($res);
    }

    public static function getSingle($id)
    {
        $res = self::query(
            "SELECT p_id, p_type, p_url, p_title, p_html, p_json, p_parameters, p_created_at, p_updated_at
            FROM page
            WHERE p_id='?'",
            $id
        );
        return self::getSingleRow($res);
    }

    public static function getByUrl($url)
    {
        $res = self::query(
            "SELECT p_id, p_type, p_url, p_title, p_html, p_json, p_parameters, p_created_at, p_updated_at
            FROM page
            WHERE p_url='?'",
            $url
        );
        return self::getSingleRow($res);
    }

    public static function add($type, $url, $title, $html, $json, $parameters)
    {
        self::query(
            "INSERT INTO page
             (p_type, p_url, p_title, p_html, p_json, p_parameters, p_created_at) VALUES ('?', NOW())",
            $type,
            $url,
            $title,
            $html,
            $json,
            $parameters
        );
        return self::getInsertId();
    }

    public static function edit($id, $url, $title, $html, $json, $parameters)
    {
        self::query(
            "UPDATE page SET p_url='?', p_title='?', p_html='?', p_json='?', p_parameters='?'
             WHERE p_id='?'",
            $url,
            $title,
            $html,
            $json,
            $parameters,
            $id
        );
    }

    public static function remove($id)
    {
        self::query("DELETE FROM page WHERE p_id='?'", $id);
    }
}
