<?php
namespace TKOlomouc\Model;

use TKOlomouc\Model\Database\Adapter;

class DBAktuality extends Adapter
{
    public static function getAktuality($kat = 0, $kdo = 0) {
        list($kat) = self::escapeArray(array($kat));

        $res = self::query(
            "SELECT *
            FROM aktuality
            WHERE 1=1" . ($kat != 0 ? " AND at_kat='$kat'" : '') .
            ($kdo > 0 ? " AND at_kdo='$kdo'" : '') .
            " ORDER BY at_timestamp_add DESC"
        );
        return self::getArray($res);
    }
    public static function getSingleAktualita($id) {
        list($id) = self::escapeArray(array($id));

        $res = self::query(
            "SELECT *
            FROM aktuality
            WHERE at_id='$id'"
        );
        return self::getSingleRow($res);
    }
    public static function addAktualita($kdo, $kat, $jmeno, $text, $preview, $foto, $foto_main) {
        list($kdo, $kat, $jmeno, $text, $preview, $foto, $foto_main) =
            self::escapeArray(array($kdo, $kat, $jmeno, $text, $preview, $foto, $foto_main));

        self::query(
            "INSERT INTO aktuality (at_kdo,at_kat,at_jmeno,at_text,at_preview,at_foto,at_foto_main,at_timestamp_add)
            VALUES ('$kdo','$kat','$jmeno','$text','$preview','$foto','$foto_main',NOW())"
        );
        return self::getInsertId();
    }
    public static function editAktualita($id, $kat, $jmeno, $text, $preview, $foto, $foto_main) {
        list($id, $kat, $jmeno, $text, $preview, $foto, $foto_main) =
            self::escapeArray(array($id, $kat, $jmeno, $text, $preview, $foto, $foto_main));

        self::query(
            "UPDATE aktuality SET at_kat='$kat',at_jmeno='$jmeno',at_text='$text',
            at_preview='$preview',at_foto='$foto',at_foto_main='$foto_main'
            WHERE at_id='$id'"
        );
    }
    public static function removeAktualita($id) {
        list($id) = self::escapeArray(array($id));

        self::query(
            "DELETE FROM aktuality WHERE at_id='$id'"
        );
    }
    public static function getAktualityFoto($id) {
        list($id) = self::escapeArray(array($id));

        $res = self::query(
            "SELECT * FROM aktuality_foto WHERE af_id_rodic='$id'"
        );
        return self::getArray($res);
    }
}