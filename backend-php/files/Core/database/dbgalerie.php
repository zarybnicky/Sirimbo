<?php
class DBGalerie extends Database
{
    public static function getFotky($dir = null)
    {
        return self::queryArray(
            'SELECT * FROM galerie_foto'
            . ($dir !== null ? " WHERE gf_id_rodic='$dir'" : '')
            . ' ORDER BY gf_id DESC'
        );
    }

    private static function _recursiveChildren(&$dirs, &$out, $dirId, $count)
    {
        if (empty($dirs)) {
            return;
        }
        for ($i = 0; $i < $count; $i++) {
            if (!isset($dirs[$i]) || $dirs[$i]['gd_id_rodic'] != $dirId) {
                continue;
            }
            $out[] = $dirs[$i];
            $id = $dirs[$i]['gd_id'];
            unset($dirs[$i]);
            self::_recursiveChildren($dirs, $out, $id, $count);
        }
    }

    public static function getDirs()
    {
        $array = self::queryArray('SELECT * FROM galerie_dir ORDER BY gd_level');
        $out = [];
        self::_recursiveChildren($array, $out, 1, count($array));
        return $out;
    }

    public static function getSingleDir($id)
    {
        return self::querySingle("SELECT * FROM galerie_dir WHERE gd_id='?'", $id);
    }

    public static function getSingleFoto($id)
    {
        return self::querySingle("SELECT * FROM galerie_foto WHERE gf_id='?'", $id);
    }
}
