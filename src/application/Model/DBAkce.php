<?php
namespace TKOlomouc\Model;

use TKOlomouc\Model\Database\Adapter;
use TKOlomouc\Model\Database\Pagable;

class DBAkce extends Adapter implements Pagable
{
    public static function getAkce($onlyVisible = false)
    {
        $res = self::query(
            'SELECT * FROM akce'
            . ($onlyVisible ? " WHERE a_visible='1'" : '') . ' ORDER BY a_od'
        );
        return self::getArray($res);
    }

    public static function getPage($offset, $count, $options = '')
    {
        list($offset, $count, $options) = self::escape($offset, $count, $options);

        $res = self::query(
            "SELECT * FROM akce "
            . ($options ? $options : '') . " LIMIT $offset,$count"
        );
        return self::getArray($res);
    }

    public static function getCount($options = null)
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
            "SELECT * FROM akce WHERE a_id='$id'"
            . ($onlyVisible ? " AND a_visible='1'" : '') . ' ORDER BY a_od'
        );
        if (!$res) {
            return false;
        } else {
            return self::getSingleRow($res);
        }
    }

    public static function getWithItems($visibleOnly = false)
    {
        $res = self::query(
            "SELECT *
            FROM akce
            LEFT JOIN akce_item ON a_id=ai_id_rodic
            LEFT JOIN users ON ai_user=u_id"
            . ($visibleOnly ? " WHERE a_visible='1'" : '')
            . " ORDER BY a_od,u_prijmeni"
        );
        return self::getArray($res);
    }

    public static function getAkceItems($id)
    {
        list($id) = self::escape($id);

        $res = self::query(
            "SELECT *
            FROM akce_item
            LEFT JOIN users ON ai_user=u_id
            WHERE ai_id_rodic='$id' ORDER BY u_prijmeni"
        );
        return self::getArray($res);
    }

    public static function addAkce($jmeno, $kde, $info, $od, $do, $kapacita, $dokumenty, $lock, $visible)
    {
        list($jmeno, $kde, $info, $od, $do, $kapacita, $dokumenty, $lock, $visible) =
            self::escape($jmeno, $kde, $info, $od, $do, $kapacita, $dokumenty, $lock, $visible);

        self::query(
            "INSERT INTO akce
            (a_jmeno,a_kde,a_info,a_od,a_do,a_kapacita,a_dokumenty,a_lock,a_visible)
            VALUES ('$jmeno','$kde','$info','$od','$do','$kapacita','$dokumenty','$lock','$visible')"
        );

        return self::getInsertId();
    }

    public static function editAkce($id, $jmeno, $kde, $info, $od, $do, $kapacita, $dokumenty, $lock, $visible)
    {
        list($id, $jmeno, $kde, $info, $od, $do, $kapacita, $dokumenty, $lock, $visible) =
            self::escape($id, $jmeno, $kde, $info, $od, $do, $kapacita, $dokumenty, $lock, $visible);

        self::query(
            "UPDATE akce
            SET a_jmeno='$jmeno',a_kde='$kde',a_info='$info',a_od='$od',a_do='$do',
                a_kapacita='$kapacita',a_dokumenty='$dokumenty',a_lock='$lock',a_visible='$visible'
            WHERE a_id='$id'"
        );

        return true;
    }

    public static function removeAkce($id)
    {
        list($id) = self::escapeArray(array($id));

        self::query("DELETE FROM akce WHERE a_id='$id'");
        self::query("DELETE FROM akce_item WHERE ai_id_rodic='$id'");

        return true;
    }

    public static function signUp($uid, $pid, $rok_narozeni)
    {
        list($uid, $pid, $rok_narozeni) = self::escape($uid, $pid, $rok_narozeni);

        self::query(
            "INSERT INTO akce_item (ai_id_rodic,ai_user,ai_rok_narozeni)
            VALUES ('$pid','$uid','$rok_narozeni')"
        );

        return true;
    }

    public static function signOut($uid, $pid)
    {
        list($uid, $pid) = self::escape($uid, $pid);

        self::query("DELETE FROM akce_item WHERE ai_user='$uid' AND ai_id_rodic='$pid'");

        return true;
    }

    public static function getAkceName($id)
    {
        list($id) = self::escape($id);

        $res = self::query("SELECT a_jmeno FROM akce WHERE a_id='$id'");

        if (!$res) {
            return false;
        } else {
            $row = self::getSingleRow($res);
            return $row["a_jmeno"];
        }
    }

    public static function addAkceItem($p_id, $u_id, $rok)
    {
        list($p_id, $u_id, $rok) = self::escape($p_id, $u_id, $rok);

        self::query(
            "INSERT INTO akce_item
            (ai_id_rodic,ai_user,ai_rok_narozeni)
            VALUES ('$p_id','$u_id','$rok')"
        );

        return true;
    }

    public static function editAkceItem($id, $u_id, $rok)
    {
        list($id, $u_id, $rok) = self::escape($id, $u_id, $rok);

        self::query(
            "UPDATE akce_item
            SET ai_user='$u_id',ai_rok_narozeni='$rok'
            WHERE ai_id='$id'"
        );

        return true;
    }

    public static function removeAkceItem($id)
    {
        list($id) = self::escape($id);

        self::query("DELETE FROM akce_item WHERE ai_id='$id'");

        return true;
    }

    public static function isUserSignedUp($a_id, $u_id)
    {
        list($a_id, $u_id) = self::escape($a_id, $u_id);

        $res = self::query("SELECT ai_id FROM akce_item WHERE ai_id_rodic='$a_id' AND ai_user='$u_id'");

        if (!$res) {
            return false;
        } else {
            $row = self::getSingleRow($res);
            return (bool)$row["ai_id"];
        }
    }
}
