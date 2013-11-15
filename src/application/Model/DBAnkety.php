<?php
namespace TKOlomouc\Model;

use TKOlomouc\Model\Database\Adapter;

class DBAnkety extends Adapter
{
    public static function getAnkety() {
        $res = self::query('SELECT * FROM ankety');
        return self::getArray($res);
    }

    public static function getSingleAnketa($id) {
        list($id) = self::escape($id);

        $res = self::query("SELECT * FROM ankety WHERE ak_id='$id'");
        return self::getSingleRow($res);
    }

    public static function getLatestAnketa() {
        $res = self::query("SELECT * FROM ankety ORDER BY ak_id DESC LIMIT 1");
        return self::getSingleRow($res);
    }

    public static function getVisibleAnketyWithItems() {
        return self::getAnketyWithItems(true);
    }

    public static function getAvailableAnketyWithItems($ip) {
        return self::getAnketyWithItems(true, $ip);
    }

    public static function getAnketyWithItems($visible = false, $ip = 0) {
        list($visible, $ip) = self::escape($visible, $ip);

        $res = self::query(
            "SELECT *
            FROM ankety AS ak
                LEFT JOIN ankety_item AS aki ON ak_id=aki_id_rodic
            WHERE 1=1" . ($visible ? " AND ak_visible='1'" : "") . ($ip > 0 ?
            " AND NOT EXISTS (SELECT akp_id FROM ankety_ip WHERE akp_id_rodic=ak.ak_id AND akp_ip=INET_ATON('$ip'))" : "")
        );
        $array = self::getArray($res);
        $result = array();

        if (!$array)
            return $result;

        $latest = -1;
        foreach ($array as $row) {
            if ($latest == $row['ak_id'])  {
                $result[count($result)-1]['items'][] = array(
                    'aki_id' => $row['aki_id'],
                    'aki_text' => $row['aki_text'],
                    'aki_pocet' => $row['aki_pocet']
                );
            } else {
                $result[] = array(
                    'ak_id' => $row['ak_id'],
                    'ak_kdo' => $row['ak_kdo'],
                    'ak_jmeno' => $row['ak_jmeno'],
                    'ak_text' => $row['ak_text']
                );
                $result[count($result)-1]['items'][] = array(
                    'aki_id' => $row['aki_id'],
                    'aki_text' => $row['aki_text'],
                    'aki_pocet' => $row['aki_pocet']
                );
                $latest = $row['ak_id'];
            }
        }
        return $result;
    }

    public static function addAnketa($kdo, $jmeno, $text, $visible) {
        list($kdo, $jmeno, $text, $visible) =
            self::escape($kdo, $jmeno, $text, $visible);

        $res = self::query('INSERT INTO ankety (ak_kdo,ak_jmeno,ak_text,ak_visible) VALUES ' .
            "('$kdo','$jmeno','$text','$visible')");

        return Adapter::getInsertId();
    }

    public static function editAnketa($id, $jmeno, $text, $visible) {
        list($id, $jmeno, $text, $visible) =
            self::escape($id, $jmeno, $text, $visible);

        $res = self::query("UPDATE ankety SET ak_jmeno='$jmeno',ak_text='$text'," .
            "ak_visible='$visible' WHERE ak_id='$id'");
        return true;
    }

    public static function removeAnketa($id) {
        list($id) = self::escape($id);

        $res = self::query("DELETE FROM ankety WHERE ak_id='$id'");
        $res = self::query("DELETE FROM ankety_item WHERE aki_id_rodic='$id'");
        $res = self::query("DELETE FROM ankety_ip WHERE akp_id_rodic='$id'");
        return true;
    }

    public static function getAnketaItems($id) {
        list($id) = self::escape($id);

        $res = self::query("SELECT * FROM ankety_item WHERE aki_id_rodic='$id'");
        return self::getArray($res);
    }

    public static function addAnketaItem($id_rodic, $text) {
        list($id_rodic, $text) = self::escape($id_rodic, $text);

        $res = self::query("INSERT INTO ankety_item (aki_id_rodic,aki_text,aki_pocet) VALUES " .
            "('$id_rodic','$text','0')");
        return true;
    }

    public static function editAnketaItem($id, $text) {
        list($id, $text) =
            self::escape($id, $text);

        $res = self::query("UPDATE ankety_item SET aki_text='$text' WHERE aki_id='$id'");
        return true;
    }

    public static function removeAnketaItem($id) {
        list($id) = self::escape($id);

        $res = self::query("DELETE FROM ankety_item WHERE aki_id='$id'");
        return true;
    }

    public static function isUniqueIP($id, $ip) {
        list($id, $ip) = self::escape($id, $ip);

        $res = self::query("SELECT akp_id FROM ankety_ip" .
            " WHERE akp_id_rodic='$id' AND akp_ip=INET_ATON('$ip')");
        $row = self::getSingleRow($res);

        return !($row && $row['akp_id']);
    }

    public static function vote($id, $choice, $ip) {
        list($id, $choice, $ip) = self::escape($id, $choice, $ip);

        $res = self::query("UPDATE ankety_item SET aki_pocet=aki_pocet+1 WHERE aki_id='$choice'");
        $res = self::query("INSERT INTO ankety_ip (akp_id_rodic,akp_ip) VALUES" .
            " ('$id', INET_ATON('$ip')) ON DUPLICATE KEY UPDATE akp_timestamp=CURRENT_TIMESTAMP");
        return true;
    }
}