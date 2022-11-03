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
}
