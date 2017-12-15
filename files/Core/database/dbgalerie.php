<?php
class DBGalerie extends Database
{
    public static function getFotky($dir = null, $limit = -1, $offset = 0)
    {
        if ($dir !== null) {
            list($dir) = self::escape($dir);
        }
        $res = self::query(
            'SELECT * FROM galerie_foto'
            . ($dir !== null ? (' WHERE gf_id_rodic="' . $dir . '"') : '')
            . ($limit > -1 ? (' LIMIT ' . $offset . ',' . $limit) : '')
        );

        return self::getArray($res);
    }

    public static function getFotkyWithParentPath($dir = false, $limit = -1, $offset = 0)
    {
        if ($dir !== false) {
            list($dir) = self::escape($dir);
        }
        $res = self::query(
            'SELECT *,(SELECT gd_path FROM galerie_dir WHERE gd_id=gf_id_rodic) AS gf_path_rodic
            FROM galerie_foto '
            . ($dir !== false ? ('WHERE gf_id_rodic="' . $dir . '" ') : '')
            . ($limit > -1 ? ('LIMIT ' . $offset . ',' . $limit) : '')
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

    public static function getDirs($by_level = false, $sort = false)
    {
        $res = self::query(
            'SELECT * FROM galerie_dir'
            . ($by_level ? ' ORDER BY gd_level' : '')
        );
        $array = self::getArray($res);

        if ($sort) {
            $out = [];
            self::_recursiveChildren($array, $out, 0, count($array));
            $array = $out;
        }
        return $array;
    }

    public static function getDirsWithParentPath()
    {
        $res = self::query(
            'SELECT *,(SELECT gd_path FROM galerie_dir WHERE gd_id=gd.gd_id_rodic) AS gd_path_rodic
            FROM galerie_dir gd'
        );
        return self::getArray($res);
    }

    public static function getSubdirs($id)
    {
        $res = self::query("SELECT * FROM galerie_dir WHERE gd_id_rodic='?'", $id);
        return self::getArray($res);
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

    public static function editFoto($id, $path, $dir = false, $name = false)
    {
        list($id, $path, $dir, $name) = self::escape($id, $path, $dir, $name);

        self::query(
            "UPDATE galerie_foto SET gf_path='$path'"
            . ($dir != false ? ",gf_id_rodic='$dir'" : '')
            . ($name != false ? ",gf_name='$name'" : '')
            . " WHERE gf_id='$id'"
        );

        return true;
    }

    public static function editFotoReplacePath($parent, $original, $new)
    {
        self::query(
        	"UPDATE galerie_foto
            SET gf_path=REPLACE(gf_path,'?','?')
            WHERE gf_id_rodic='?'",
            $original, $new, $parent
        );
    }

    public static function removeFoto($id)
    {
        self::query("DELETE FROM galerie_foto WHERE gf_id='?'", $id);
        return true;
    }

    public static function addDir($name, $parent, $level, $hidden, $path)
    {
        self::query(
            "INSERT INTO galerie_dir
            (gd_name,gd_id_rodic,gd_level,gd_hidden,gd_path) VALUES
            ('?','?','?','?','?')",
            $name, $parent, $level, $hidden, $path
        );
        return true;
    }

    public static function editDir($id, $name, $parent, $level, $hidden, $path)
    {
        self::query(
            "UPDATE galerie_dir
            SET gd_name='?',gd_id_rodic='?',gd_level='?',gd_hidden='?',gd_path='?'
            WHERE gd_id='?'",
            $name, $parent, $level, $hidden, $path, $id
        );
    }

    public static function removeDir($id)
    {
        list($id) = self::escape($id);

        self::query("DELETE FROM galerie_dir WHERE gd_id='$id'");
        self::query("DELETE FROM galerie_dir WHERE gd_id_rodic='$id'");
        self::query("DELETE FROM galerie_foto WHERE gf_id_rodic='$id'");

        return true;
    }

    public static function addFotoByPath($dir, $path, $name, $kdo) {
        list($dir, $path, $name, $kdo) = self::escape($dir, $path, $name, $kdo);

        if ($dir == '') {
            $dir = 0;
        } else {
            $dir = "(SELECT gd_id FROM galerie_dir gd2 WHERE gd2.gd_path='$dir')";
        }

        self::query(
            "INSERT INTO galerie_foto
            (gf_id_rodic,gf_path,gf_name,gf_kdo) VALUES
            ($dir,
            '$path','$name','$kdo')
            ON DUPLICATE KEY UPDATE gf_name='$name'"
        );
        return true;
    }

    public static function addDirByPath($name, $parent, $level, $path)
    {
        list($name, $parent, $level, $path) =
            self::escape($name, $parent, $level, $path);

        if ($parent == '') {
            $parent = 0;
        } else {
            $parent = "(SELECT gd_id FROM galerie_dir gd2 WHERE gd2.gd_path='$parent')";
        }

        self::query(
            "INSERT INTO galerie_dir
            (gd_name,gd_id_rodic,gd_level,gd_path) VALUES
            ('$name',$parent,
            '$level','$path')
            ON DUPLICATE KEY UPDATE gd_name='$name'"
        );
    }

    public static function removeDirByPath($path)
    {
        list($path) = self::escape($path);

        if (!$path)
            return;

        self::query(
            "DELETE FROM galerie_dir
            WHERE gd_id_rodic=(SELECT * FROM (SELECT gd_id FROM galerie_dir gd2 WHERE gd2.gd_path='$path') as x)"
        );
        self::query(
            "DELETE FROM galerie_foto
            WHERE gf_id_rodic=(SELECT gd_id FROM galerie_dir WHERE gd_path='$path')"
        );
        self::query(
            "DELETE FROM galerie_dir WHERE gd_path='$path'"
        );
    }

    public static function removeFotoByPath($path)
    {
        self::query("DELETE FROM galerie_foto WHERE gf_path='?'", $path);
        return true;
    }
}
