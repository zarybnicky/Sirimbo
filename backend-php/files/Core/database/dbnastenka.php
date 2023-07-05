<?php
class DBNastenka extends Database implements Pagable
{
    public static function getNastenka($offset = null, $count = null)
    {
        $res = self::query(
        "SELECT *
        FROM upozorneni
            LEFT JOIN users ON up_kdo=u_id
        ORDER BY up_timestamp_add DESC" . (($offset !== null && $count !== null) ? " LIMIT $count OFFSET $offset" : ''));
        return self::getArray($res);
    }

    public function getPage($offset, $count, $options = null)
    {
        return self::getNastenka($offset, $count);
    }

    public function getCount($options = null)
    {
        $res = self::query("SELECT COUNT(*) as count FROM upozorneni");
        return self::getSingleRow($res)['count'];
    }

    public static function getNastenkaSkupiny($id)
    {
        $res = self::query(
            "SELECT * FROM upozorneni_skupiny WHERE ups_id_rodic='?'",
            $id
        );
        return self::getArray($res);
    }

    public static function addNastenkaSkupina($rodic, $skupina, $color)
    {
        self::query(
            "INSERT INTO upozorneni_skupiny (ups_id_rodic,ups_id_skupina,ups_color)
            VALUES ('?','?','?','?')",
            $rodic,
            $skupina,
            $color
        );
    }

    public static function removeNastenkaSkupina($id)
    {
        self::query("DELETE FROM upozorneni_skupiny WHERE ups_id='?'", $id);
    }

    public static function getSingleNastenka($id)
    {
        $res = self::query(
            "SELECT * FROM upozorneni LEFT JOIN users ON up_kdo=u_id WHERE up_id='?'",
            $id
        );
        if (!$res) {
            return false;
        }
        return self::getSingleRow($res);
    }

    public static function addNastenka($userid, $nadpis, $text, $lock)
    {
        self::query(
            "INSERT INTO upozorneni
            (up_kdo,up_nadpis,up_text,up_lock,up_timestamp_add) VALUES ('?','?','?','?',NOW())",
            $userid, $nadpis, $text, $lock
        );
        return self::getInsertId();
    }

    public static function editNastenka($id, $nadpis, $text, $lock)
    {
        self::query(
            "UPDATE upozorneni SET up_nadpis='?',up_text='?',up_lock='?' WHERE up_id='?'",
            $nadpis, $text, $lock, $id
        );
        return true;
    }

    public static function removeNastenka($id)
    {
        self::query("DELETE FROM upozorneni WHERE up_id='?'", $id);
        self::query("DELETE FROM upozorneni_skupiny WHERE ups_id_rodic='?'", $id);
        return true;
    }
}
