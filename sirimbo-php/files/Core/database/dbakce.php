<?php
class DBAkce extends Database
{
    public static function getAkce($onlyVisible = false)
    {
        $res = self::query(
            'SELECT * FROM akce'
            . ($onlyVisible ? " WHERE a_visible='1'" : '') . ' ORDER BY a_do DESC, a_od DESC'
        );
        return self::getArray($res);
    }

    public static function getSingleAkce($id, $onlyVisible = false)
    {
        $res = self::query(
            "SELECT * FROM akce WHERE a_id='?'" .
            ($onlyVisible ? " AND a_visible='1'" : '') . ' ORDER BY a_od',
            $id
        );
        return $res ? self::getSingleRow($res) : false;
    }

    public static function getAkceItems($id)
    {
        $res = self::query(
            "SELECT *
            FROM akce_item
            LEFT JOIN users ON ai_user=u_id
            LEFT JOIN skupiny ON u_skupina=s_id
            WHERE ai_id_rodic='?' ORDER BY u_prijmeni",
            $id
        );
        return self::getArray($res);
    }

    public static function signUp($uid, $pid, $rok_narozeni)
    {
        self::query(
            "INSERT INTO akce_item (ai_id_rodic,ai_user,ai_rok_narozeni)" .
            " VALUES ('?','?','?')",
            $pid,
            $uid,
            $rok_narozeni
        );
        return true;
    }

    public static function signOut($uid, $pid)
    {
        self::query("DELETE FROM akce_item WHERE ai_user='?' AND ai_id_rodic='?'", $uid, $pid);
        return true;
    }

    public static function isUserSignedUp($a_id, $u_id)
    {
        $res = self::query(
            "SELECT ai_id FROM akce_item WHERE ai_id_rodic='?' AND ai_user='?'",
            $a_id,
            $u_id
        );
        if (!$res) {
            return false;
        } else {
            $row = self::getSingleRow($res);
            return (bool) ($row["ai_id"] ?? false);
        }
    }
}
