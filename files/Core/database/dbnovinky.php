<?php
class DBNovinky extends Database
{
    public static function getNovinky() {
        $res = self::query('SELECT * FROM novinky');

        return self::getArray($res);
    }

    public static function getLastNovinky($number) {
        list($number) = self::escape($number);

        $res = self::query(
                "SELECT * FROM novinky
                    LEFT JOIN users ON no_id_user=u_id
                ORDER BY no_id DESC LIMIT $number");

        return self::getArray($res);
    }

    public static function addNovinka($uid, $text) {
        list($uid, $text) = self::escape($uid, $text);
        self::query("INSERT INTO novinky (no_id_user, no_text) VALUES ('$uid','$text')");
    }

    public static function removeNovinka($id) {
        list($id) = self::escape($id);
        self::query("DELETE FROM novinky WHERE no_id=$id");
    }
}
