
<?php
class DBAktuality extends Database
{
    public static function getAktuality($kat = 0, $kdo = 0)
    {
        list($kat) = self::escape($kat);

        $res = self::query(
            "SELECT *
            FROM aktuality
            LEFT JOIN galerie_foto ON gf_id=at_foto_main
            WHERE 1=1"
            . ($kat ? " AND at_kat='$kat'" : '')
            . ($kdo ? " AND at_kdo='$kdo'" : '')
            . " ORDER BY at_timestamp_add DESC"
        );
        return self::getArray($res);
    }

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
            VALUES ('?','?','?','?','?','?','?',NOW())",
            $kdo, $kat, $jmeno, $text, $preview, $foto, $foto_main
        );
        return self::getInsertId();
    }

    public static function editAktualita($id, $kat, $jmeno, $text, $preview, $foto, $foto_main, $createdAt)
    {
        self::query(
            "UPDATE aktuality
            SET at_kat='?',at_jmeno='?',at_text='?',at_preview='?',at_foto='?',at_foto_main='?',at_timestamp_add='?'
            WHERE at_id='?'",
            $kat, $jmeno, $text, $preview, $foto, $foto_main, $createdAt, $id
        );
    }

    public static function removeAktualita($id)
    {
        self::query("DELETE FROM aktuality WHERE at_id='?'", $id);
    }
}
