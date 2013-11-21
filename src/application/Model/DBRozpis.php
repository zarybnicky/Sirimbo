<?php
namespace TKOlomouc\Model;

use TKOlomouc\Model\Database\Adapter;

class DBRozpis extends Adapter
{
    public static function getRozpis()
    {
        $res = self::query(
            "SELECT u_jmeno,u_prijmeni,r_id,r_trener,r_kde,r_datum,r_visible,r_lock
            FROM rozpis LEFT JOIN users ON r_trener=u_id
            ORDER BY r_datum"
        );
        return self::getArray($res);
    }

    public static function rozpisSignUp($rid, $uid)
    {
        list($rid, $uid) = self::escape($rid, $uid);
        if (self::isRozpisFree($rid)) {
            $res = self::query("UPDATE rozpis_item SET ri_partner='$uid' WHERE ri_id='$rid'");
            return true;
        } else {
            return false;
        }
    }

    public static function rozpisSignOut($rid)
    {
        list($rid) = self::escape($rid);
        if (!self::isRozpisFree($rid)) {
            $res = self::query("UPDATE rozpis_item SET ri_partner='' WHERE ri_id='$rid'");
            return true;
        } else {
            return false;
        }
    }

    public static function getRozpisItem($rid)
    {
        list($rid) = self::escape($rid);

        $res = self::query(
            "SELECT p_id,u_id,u_login,u_jmeno,u_prijmeni,ri_id,ri_id_rodic,ri_partner,
                ri_od,ri_do,ri_lock
            FROM rozpis_item
                LEFT JOIN pary ON ri_partner=p_id
                LEFT JOIN users ON p_id_partner=u_id
            WHERE ri_id_rodic='$rid'
            ORDER BY ri_od"
        );
        return self::getArray($res);
    }

    public static function getRozpisItemLesson($ri_id)
    {
        list($ri_id) = self::escape($ri_id);

        $res = self::query(
            "SELECT u_id,u_login,u_jmeno,u_prijmeni,r_trener,ri_id,ri_id_rodic,ri_partner,
                ri_od,ri_do,ri_lock
            FROM rozpis_item
                LEFT JOIN users ON ri_partner=u_id
                LEFT JOIN rozpis ON ri_id_rodic=r_id
            WHERE ri_id='$ri_id'"
        );
        return self::getSingleRow($res);
    }

    public static function isRozpisFree($rid)
    {
        list($rid) = self::escape($rid);

        $res = self::query("SELECT ri_partner FROM rozpis_item WHERE ri_id='$rid'");
        if (!$res) {
            return false;
        } else {
            $row = self::getSingleRow($res);
            return !(bool)$row["ri_partner"];
        }
    }

    public static function getSingleRozpis($id)
    {
        list($id) = self::escape($id);

        $res = self::query(
            "SELECT r_id,r_trener,u_jmeno,u_prijmeni,r_kde,r_datum,r_visible,r_lock
            FROM rozpis LEFT JOIN users ON r_trener=u_id
            WHERE r_id='$id'"
        );
        if (!$res) {
            return false;
        } else {
            return self::getSingleRow($res);
        }
    }

    public static function getRozpisTrener($id)
    {
        list($id) = self::escape($id);

        $res = self::query(
            "SELECT * FROM users
            WHERE u_id=(SELECT r_trener FROM rozpis WHERE r_id='$id')"
        );
        if (!$res) {
            return false;
        } else {
            return self::getSingleRow($res);
        }
    }

    public static function addRozpis($trener, $kde, $datum, $visible, $lock)
    {
        list($trener, $kde, $datum, $visible, $lock) =
            self::escape($trener, $kde, $datum, $visible, $lock);

        self::query(
            "INSERT INTO rozpis (r_trener,r_kde,r_datum,r_visible,r_lock)
            VALUES ('$trener','$kde','$datum','$visible','$lock')"
        );

        return true;
    }

    public static function editRozpis($id, $trener, $kde, $datum, $visible, $lock)
    {
        list($id, $trener, $kde, $datum, $visible, $lock) =
            self::escape($id, $trener, $kde, $datum, $visible, $lock);

        self::query(
            "UPDATE rozpis SET r_trener='$trener',r_kde='$kde',r_datum='$datum',
            r_visible='$visible',r_lock='$lock' WHERE r_id='$id'"
        );

        return true;
    }

    public static function removeRozpis($id)
    {
        list($id) = self::escape($id);

        self::query("DELETE FROM rozpis WHERE r_id='$id'");
        self::query("DELETE FROM rozpis_item WHERE ri_id_rodic='$id'");

        return true;
    }

    public static function isRozpisLocked($id)
    {
        list($id) = self::escape($id);

        $res = self::query("SELECT r_lock FROM rozpis WHERE r_id='$id'");
        if (!$res) {
            return false;
        } else {
            $row = self::getSingleRow($res);
            return (bool)$row["r_lock"];
        }
    }

    public static function isRozpisVisible($id)
    {
        list($id) = self::escape($id);

        $res = self::query("SELECT r_visible FROM rozpis WHERE r_id='$id'");
        if (!$res) {
            return false;
        } else {
            $row = self::getSingleRow($res);
            return (bool)$row["r_visible"];
        }
    }

    public static function addRozpisItem($parent_id, $user_id, $od, $do, $lock)
    {
        list($parent_id, $user_id, $od, $do, $lock) =
            self::escape($parent_id, $user_id, $od, $do, $lock);

        self::query(
            "INSERT INTO rozpis_item (ri_id_rodic,ri_partner,ri_od,ri_do,ri_lock)
            VALUES ('$parent_id','$user_id','$od','$do','$lock')
            ON DUPLICATE KEY UPDATE ri_partner='$user_id',ri_do='$do',ri_lock='$lock'"
        );

        return true;
    }

    public static function editRozpisItem($id, $partner, $od, $do, $lock)
    {
        list($id, $partner, $od, $do, $lock)
            = self::escape($id, $partner, $od, $do, $lock);

        $res = self::query(
            "SELECT ri_id,ri_id_rodic
            FROM rozpis_item
            WHERE ri_od='$od' AND
                ri_id_rodic=(SELECT ri_id_rodic FROM rozpis_item WHERE ri_id='$id')"
        );
            //Finds conflicting rozpis

        if (!$res) {
            return false;
        } else {
            $row = self::getSingleRow($res);
        }
        if ($row['ri_id'] && $row['ri_id'] != $id) {
            //if there is a conflicting rozpis:
            self::removeRozpisItem($id);
            self::addRozpisItem($row['ri_id_rodic'], $partner, $od, $do, $lock);
        } else {
            self::query(
                "UPDATE rozpis_item
                SET ri_partner='$partner',ri_od='$od',ri_do='$do',ri_lock='$lock'
                WHERE ri_id='$id'"
            );
        }

        return true;
    }

    public static function removeRozpisItem($id)
    {
        list($id) = self::escape($id);

        self::query("DELETE FROM rozpis_item WHERE ri_id='$id'");

        return true;
    }
}
