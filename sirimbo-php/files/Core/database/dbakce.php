<?php
class DBAkce extends Database implements Pagable
{
    public static function getAkce($onlyVisible = false)
    {
        $res = self::query(
            'SELECT * FROM akce'
            . ($onlyVisible ? " WHERE a_visible='1'" : '') . ' ORDER BY a_do DESC, a_od DESC'
        );
        return self::getArray($res);
    }

    public function getPage($offset, $count, $options = '')
    {
        list($offset, $count, $options) = self::escape($offset, $count, $options);

        $res = self::query(
            "SELECT * FROM akce " .
            ($options ? $options : '') . " LIMIT $offset,$count"
        );
        return self::getArray($res);
    }

    public function getCount($options = null)
    {
        $res = self::query("SELECT COUNT(*) FROM akce");
        if (!$res) {
            return false;
        } else {
            $row = self::getSingleRow($res);
            return $row['COUNT(*)'];
        }
    }

    public static function getSingleAkce($id, $onlyVisible = false)
    {
        list($id) = self::escape($id);

        $res = self::query(
            "SELECT * FROM akce WHERE a_id='$id'" .
            ($onlyVisible ? " AND a_visible='1'" : '') . ' ORDER BY a_od'
        );
        if (!$res) {
            return false;
        } else {
            return self::getSingleRow($res);
        }
    }

    public static function getWithItemCount()
    {
        $res = self::query(
            'SELECT *, (SELECT COUNT(*) FROM akce_item WHERE a_id=ai_id_rodic) as a_obsazeno
             FROM akce ORDER BY a_od DESC'
        );
        return self::getArray($res);
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

    public static function addAkce($jmeno, $kde, $info, $od, $do, $kapacita, $dokumenty, $lock, $visible)
    {
        self::query(
            "INSERT INTO akce" .
            " (a_jmeno,a_kde,a_info,a_od,a_do,a_kapacita,a_dokumenty,a_lock,a_visible)" .
            " VALUES ('?','?','?','?','?','?','?','?','?')",
            $jmeno, $kde, $info, $od, $do, $kapacita, $dokumenty, $lock, $visible
        );
        return self::getInsertId();
    }

    public static function editAkce($id, $jmeno, $kde, $info, $od, $do, $kapacita, $dokumenty, $lock, $visible)
    {
        self::query(
            "UPDATE akce SET a_jmeno='?',a_kde='?',a_info='?',a_od='?',a_do='?'," .
            "a_kapacita='?',a_dokumenty='?',a_lock='?',a_visible='?' WHERE a_id='?'",
            $jmeno, $kde, $info, $od, $do, $kapacita, $dokumenty, $lock, $visible, $id
        );
        return true;
    }

    public static function removeAkce($id)
    {
        self::query("DELETE FROM akce WHERE a_id='?'", $id);
        self::query("DELETE FROM akce_item WHERE ai_id_rodic='?'", $id);
        return true;
    }

    public static function signUp($uid, $pid, $rok_narozeni)
    {
        self::query(
            "INSERT INTO akce_item (ai_id_rodic,ai_user,ai_rok_narozeni)" .
            " VALUES ('?','?','?')",
            $pid, $uid, $rok_narozeni
        );
        return true;
    }

    public static function signOut($uid, $pid)
    {
        self::query("DELETE FROM akce_item WHERE ai_user='?' AND ai_id_rodic='?'", $uid, $pid);
        return true;
    }

    public static function addAkceItem($p_id, $u_id, $rok)
    {
        self::query(
            "INSERT INTO akce_item (ai_id_rodic,ai_user,ai_rok_narozeni)" .
            " VALUES ('?','?','?')",
            $p_id, $u_id, $rok
        );
        return true;
    }

    public static function editAkceItem($id, $u_id, $rok)
    {
        self::query(
            "UPDATE akce_item SET ai_user='?',ai_rok_narozeni='?' WHERE ai_id='?'",
            $u_id,
            $rok,
            $id,
        );
        return true;
    }

    public static function removeAkceItem($id)
    {
        self::query("DELETE FROM akce_item WHERE ai_id='?'", $id);
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
