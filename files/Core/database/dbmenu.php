<?php
class DBMenu extends Database
{
    public static function getAll()
    {
        $res = self::query(
            'SELECT m_id, m_key, m_title, m_content, m_updated_at
            FROM menu ORDER BY m_updated_at DESC'
        );
        return self::getArray($res);
    }

    public static function edit($id, $content)
    {
        self::query(
            "UPDATE menu SET m_content='?'
             WHERE m_id='?'",
            $content,
            $id
        );
    }
}
