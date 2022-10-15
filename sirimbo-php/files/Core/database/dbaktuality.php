<?php
class DBAktuality extends Database
{
    public static function getSingleAktualita($id)
    {
        $res = self::query(
            "SELECT *
            FROM aktuality
            LEFT JOIN galerie_foto ON gf_id=at_foto_main
            WHERE at_id='?'",
            $id
        );
        return self::getSingleRow($res);
    }

    public static function addAktualita($kdo, $kat, $jmeno, $text, $preview, $foto, $foto_main)
    {
        self::query(
            "INSERT INTO aktuality (at_kdo,at_kat,at_jmeno,at_text,at_preview,at_foto,at_foto_main,at_timestamp_add)
            VALUES ('?','?','?','?','?','?'," . ($foto_main ? "'$foto_main'" : 'NULL') . ",NOW())",
            $kdo,
            $kat,
            $jmeno,
            $text,
            $preview,
            $foto,
        );
        return self::getInsertId();
    }

    public static function editAktualita($id, $kat, $jmeno, $text, $preview, $foto, $foto_main, $createdAt)
    {
        self::query(
            "UPDATE aktuality
            SET at_kat='?',at_jmeno='?',at_text='?',at_preview='?',at_foto='?',at_foto_main="
            . ($foto_main ? "'$foto_main'" : 'NULL') . ",at_timestamp_add='?'
            WHERE at_id='?'",
            $kat,
            $jmeno,
            $text,
            $preview,
            $foto,
            $createdAt,
            $id,
        );
    }

    public static function removeAktualita($id)
    {
        self::query("DELETE FROM aktuality WHERE at_id='?'", $id);
    }
}
