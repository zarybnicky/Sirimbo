<?php
class DBAktuality extends Database
{
    public static function getAktuality($kat = 0, $kdo = 0)
    {
        return \Database::queryArray(
            "SELECT * FROM aktuality LEFT JOIN galerie_foto ON gf_id=at_foto_main
            WHERE 1=1"
            . ($kat ? " AND at_kat='$kat'" : '')
            . ($kdo ? " AND at_kdo='$kdo'" : '')
            . " ORDER BY at_timestamp_add DESC"
        );
    }

    public static function getSingleAktualita($id)
    {
        return \Database::querySingle(
            "SELECT * FROM aktuality LEFT JOIN galerie_foto ON gf_id=at_foto_main WHERE at_id='?'",
            $id
        );
    }
}
