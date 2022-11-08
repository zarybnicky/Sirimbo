<?php
class DBGalerie extends Database
{
    public static function getFotky($dir = null)
    {
        $res = self::query(
            'SELECT * FROM galerie_foto'
            . ($dir !== null ? " WHERE gf_id_rodic='$dir'" : '')
            . ' ORDER BY gf_id DESC'
        );
        return self::getArray($res);
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
        $res = self::query('SELECT * FROM galerie_dir ORDER BY gd_level');
        $array = self::getArray($res);
        $out = [];
        self::_recursiveChildren($array, $out, 1, count($array));
        return $out;
    }

    public static function getSingleDir($id)
    {
        $res = self::query("SELECT * FROM galerie_dir WHERE gd_id='?'", $id);
        return self::getSingleRow($res);
    }

    public static function getSingleFoto($id)
    {
        $res = self::query("SELECT * FROM galerie_foto WHERE gf_id='?'", $id);
        return self::getSingleRow($res);
    }

    public static function addFoto($dir, $path, $name, $kdo)
    {
        self::query(
            "INSERT INTO galerie_foto
            (gf_id_rodic,gf_path,gf_name,gf_kdo) VALUES
            ('?','?','?','?')",
            $dir, $path, $name, $kdo
        );
        return true;
    }

    public static function removeFoto($id)
    {
        self::query("DELETE FROM galerie_foto WHERE gf_id='?'", $id);
        return true;
    }

    public static function removeDir($id)
    {
        self::query("DELETE FROM galerie_dir WHERE gd_id='?'", $id);
        self::query("DELETE FROM galerie_dir WHERE gd_id_rodic='?'", $id);
        self::query("DELETE FROM galerie_foto WHERE gf_id_rodic='?'", $id);
        return true;
    }
}
