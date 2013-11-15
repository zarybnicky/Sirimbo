<?php
namespace TKOlomouc\Model;

use TKOlomouc\Model\Database\Adapter;

class DBNabidka extends Adapter
{
    public static function getNabidka() {
        $res = self::query("SELECT u_id,u_jmeno,u_prijmeni,nabidka.*" .
            " FROM nabidka LEFT JOIN users ON n_trener=u_id ORDER BY n_od");
        return self::getArray($res);
    }

    public static function getSingleNabidka($id) {
        list($id) = self::escapeArray(array($id));

        $res = self::query(
        "SELECT n_id,u_jmeno,u_prijmeni,nabidka.*
        FROM nabidka
            LEFT JOIN users ON n_trener=u_id
        WHERE n_id='$id'");
        if (!$res) {
            return false;
        } else {
            return self::getSingleRow($res);
        }
    }

    public static function addNabidka($trener, $pocet_hod, $max_hod, $od, $do, $visible, $lock) {
        list($trener, $pocet_hod, $max_hod, $od, $do, $visible, $lock) =
            self::escapeArray(array($trener, $pocet_hod, $max_hod, $od, $do, $visible, $lock));

        self::query("INSERT INTO nabidka (n_trener,n_pocet_hod,n_max_pocet_hod,n_od,n_do,n_visible,n_lock) VALUES " .
            "('$trener','$pocet_hod','$max_hod','$od','$do','$visible','$lock')");

        return true;
    }

    public static function editNabidka($id, $trener, $pocet_hod, $max_hod, $od, $do, $visible, $lock) {
        list($id, $trener, $pocet_hod, $max_hod, $od, $do, $visible, $lock) =
            self::escapeArray(array($id, $trener, $pocet_hod, $max_hod, $od, $do, $visible, $lock));

        self::query("UPDATE nabidka SET n_trener='$trener',n_pocet_hod='$pocet_hod',n_max_pocet_hod='$max_hod',n_od='$od'," .
            "n_do='$do',n_visible='$visible',n_lock='$lock' WHERE n_id='$id'");

        return true;
    }

    public static function removeNabidka($id) {
        list($id) = self::escapeArray(array($id));

        self::query("DELETE FROM nabidka WHERE n_id='$id'");
        self::query("DELETE FROM nabidka_item WHERE ni_id_rodic='$id'");

        return true;
    }

    public static function getNabidkaItem($parent_id) {
        list($parent_id) = self::escapeArray(array($parent_id));

        $res = self::query(
        "SELECT p_id,u_id,u_jmeno,u_prijmeni,nabidka_item.*
        FROM nabidka_item
            LEFT JOIN pary ON ni_partner=p_id
            LEFT JOIN users ON p_id_partner=u_id
        WHERE ni_id_rodic='$parent_id'");
        return self::getArray($res);
    }

    public static function getNabidkaItemLessons($id) {
        list($id) = self::escapeArray(array($id));

        $res = self::query("SELECT SUM(ni_pocet_hod) FROM nabidka_item WHERE ni_id_rodic='$id'");
        if (!$res) {
            return false;
        } else {
            $row = self::getSingleRow($res);
            return $row["SUM(ni_pocet_hod)"];
        }
    }

    public static function getNabidkaMaxItems($id) {
        list($id) = self::escapeArray(array($id));

        $res = self::query("SELECT MAX(ni_pocet_hod) FROM nabidka_item WHERE ni_id_rodic='$id'");
        if (!$res) {
            return false;
        } else {
            $row = self::getSingleRow($res);
            return $row["MAX(ni_pocet_hod)"];
        }
    }

    public static function getNabidkaLessons($parent_id, $u_id) {
        list($parent_id, $u_id) = self::escapeArray(array($parent_id, $u_id));

        $res = self::query("SELECT ni_pocet_hod FROM nabidka_item WHERE ni_id_rodic='$parent_id' AND " .
            "ni_partner='$u_id'");
        if (!$res) {
            return false;
        } else {
            $row = self::getSingleRow($res);
            return $row["ni_pocet_hod"];
        }
    }

    public static function addNabidkaItemLessons($user_id, $parent_id, $pocet_hod) {
        list($user_id, $parent_id, $pocet_hod) = self::escapeArray(array($user_id, $parent_id, $pocet_hod));

        self::query("INSERT INTO nabidka_item (ni_partner,ni_id_rodic,ni_pocet_hod)" .
            " VALUES ('$user_id','$parent_id','$pocet_hod')" .
            " ON DUPLICATE KEY UPDATE ni_pocet_hod=ni_pocet_hod+'$pocet_hod'");

        return true;
    }

    public static function editNabidkaItem($id, $partner, $pocet_hod) {
        list($id, $partner, $pocet_hod) = self::escapeArray(array($id, $partner, $pocet_hod));

        $res = self::query(
        "SELECT ni_id,ni_id_rodic FROM nabidka_item
        WHERE ni_partner='$partner' AND
            ni_id_rodic=(SELECT ni_id_rodic FROM nabidka_item WHERE ni_id='$id')");
        //Hledá konfliktní nabidku

        if (!$res) {
            return false;
        } else {
            $row = self::getSingleRow($res);
        }
        if ($row['ni_id'] && $row['ni_id'] != $id) { //If there is a conflicting nabidka
            self::removeNabidkaItemByID($id);
            self::addNabidkaItemLessons($partner, $row['ni_id_rodic'], $pocet_hod);
        } else {
            self::query(
            "UPDATE nabidka_item
            SET ni_partner='$partner',ni_pocet_hod='$pocet_hod'
            WHERE ni_id='$id'");
        }

        return true;
    }

    public static function removeNabidkaItem($parent_id, $u_id) {
        list($parent_id, $u_id) = self::escapeArray(array($parent_id, $u_id));

        self::query("DELETE FROM nabidka_item WHERE ni_id_rodic='$parent_id' AND " .
            "ni_partner='$u_id'");

        return true;
    }

    public static function removeNabidkaItemByID($id) {
        list($id) = self::escapeArray(array($id));

        self::query("DELETE FROM nabidka_item WHERE ni_id='$id'");

        return true;
    }
}
?>