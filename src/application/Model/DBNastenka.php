<?php
namespace TKOlomouc\Model;

use TKOlomouc\Model\Database\Adapter;
use TKOlomouc\Model\Database\Pagable;

class DBNastenka extends Adapter implements Pagable
{
    public static function getInstance()
    {
        return new self();
    }

    public static function getNastenka($offset = null, $count = null)
    {
        $res = self::query(
            "SELECT *
            FROM upozorneni
                LEFT JOIN users ON up_kdo=u_id
            ORDER BY up_timestamp_add DESC"
            . (($offset !== null && $count !== null) ? " LIMIT $offset,$count" : '')
        );
        return self::getArray($res);
    }

    public static function getPage($offset, $count, $options = null)
    {
        return self::getNastenka($offset, $count);
    }

    public static function getCount($options = null)
    {
        return self::getNastenkaCount();
    }

    public static function getNastenkaCount()
    {
        $res = self::query("SELECT COUNT(*) FROM upozorneni");

        $res = self::getSingleRow($res);
        return $res['COUNT(*)'];
    }

    public static function getNastenkaSkupiny($id)
    {
        list($id) = self::escape($id);

        $res = self::query("SELECT * FROM upozorneni_skupiny WHERE ups_id_rodic='$id'");
        return self::getArray($res);
    }

    public static function addNastenkaSkupina($rodic, $skupina, $color, $popis)
    {
        list($rodic, $skupina, $color, $popis) =
            self::escape($rodic, $skupina, $color, $popis);

        self::query(
            "INSERT INTO upozorneni_skupiny
            (ups_id_rodic,ups_id_skupina,ups_color,ups_popis)
            VALUES ('$rodic','$skupina','$color','$popis')"
        );
    }

    public static function removeNastenkaSkupina($id)
    {
        list($id) = self::escape($id);

        self::query("DELETE FROM upozorneni_skupiny WHERE ups_id='$id'");
    }

    public static function getNastenkaUserName($id)
    {
        list($id) = self::escape($id);

        $res = self::query(
            "SELECT u_login
            FROM upozorneni LEFT JOIN users ON up_kdo=u_id
            WHERE up_id='$id'"
        );
        if (!$res) {
            return false;
        } else {
            $row = self::getSingleRow($res);
            return $row["u_login"];
        }
    }

    public static function getSingleNastenka($id)
    {
        list($id) = self::escape($id);

        $res = self::query(
            "SELECT *
            FROM upozorneni LEFT JOIN users ON up_kdo=u_id
            WHERE up_id='$id'"
        );
        if (!$res) {
            return false;
        } else {
            return self::getSingleRow($res);
        }
    }

    public static function isNastenkaLocked($id)
    {
        list($id) = self::escapearray($id);

        $res = self::query("SELECT up_lock FROM upozorneni WHERE up_id='$id'");

        if (!$res) {
            return false;
        } else {
            $row = self::getSingleRow($res);
            return $row["up_lock"];
        }
    }

    public static function addNastenka($userid, $nadpis, $text, $lock)
    {
        list($userid, $nadpis, $text, $lock) =
            self::escape($userid, $nadpis, $text, $lock);

        self::query(
            "INSERT INTO upozorneni
            (up_kdo,up_nadpis,up_text,up_lock,up_timestamp_add)
            VALUES ('$userid','$nadpis','$text','$lock',NOW())"
        );
        return self::getInsertId();
    }

    public static function editNastenka($id, $nadpis, $text, $lock)
    {
        list($id, $nadpis, $text, $lock) =
            self::escape($id, $nadpis, $text, $lock);

        self::query(
            "UPDATE upozorneni
            SET up_nadpis='$nadpis',up_text='$text',up_lock='$lock'
            WHERE up_id='$id'"
        );

        return true;
    }

    public static function removeNastenka($id)
    {
        list($id) = self::escape($id);

        self::query("DELETE FROM upozorneni WHERE up_id='$id'");
        self::query("DELETE FROM upozorneni_skupiny WHERE ups_id_rodic='$id'");

        return true;
    }
}
