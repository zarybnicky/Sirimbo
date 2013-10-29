<?php
class DBGalerie extends Database
{
    public static function getFotky($dir = null, $limit = -1, $offset = 0) {
        if ($dir !== null)
            list($dir) = self::escapeArray(array($dir));
        $res = self::query('SELECT * FROM galerie_foto' .
            ($dir !== null ? ' WHERE gf_id_rodic="' . $dir . '"' : '') .
            (($limit > -1) ? (' LIMIT ' . $offset . ',' . $limit) : ''));

        return self::getArray($res);
    }
    public static function getFotkyWithParentPath($dir = false, $limit = -1, $offset = 0) {
        if ($dir !== false)
            list($dir) = self::escapeArray(array($dir));
        $res = self::query(
            'SELECT *,(SELECT gd_path FROM galerie_dir WHERE gd_id=gf_id_rodic) AS gf_path_rodic
            FROM galerie_foto ' . ($dir !== false ? 'WHERE gf_id_rodic="' . $dir . '"' : '') .
            (($limit > -1) ? (' LIMIT ' . $offset . ',' . $limit) : ''));

        return self::getArray($res);
    }
    private static function _recursiveChildren(&$dirs, &$out, $dirId, $count) {
        if (empty($dirs))
            return;

        for($i = 0; $i < $count; $i++) {
            if (!isset($dirs[$i]) || $dirs[$i]['gd_id_rodic'] != $dirId)
                continue;
            $out[] = $dirs[$i];
            $id = $dirs[$i]['gd_id'];
            unset($dirs[$i]);
            self::_recursiveChildren($dirs, $out, $id, $count);
        }
    }
    public static function getDirs($by_level = false, $sort = false) {
        $res = self::query('SELECT * FROM galerie_dir' . ($by_level ? ' ORDER BY gd_level' : ''));
        $array = self::getArray($res);

        if ($sort) {
            $out = array();
            self::_recursiveChildren($array, $out, 0, count($array));
            $array = $out;
        }

        return $array;
    }
    public static function getDirsWithParentPath() {
        $res = self::query(
            'SELECT *,(SELECT gd_path FROM galerie_dir WHERE gd_id=child.gd_id_rodic) AS gd_path_rodic
            FROM galerie_dir AS child');
        $array = self::getArray($res);

        return $array;
    }
    public static function getSubdirs($id) {
        list($id) = self::escapeArray(array($id));

        $res = self::query("SELECT * FROM galerie_dir WHERE gd_id_rodic='$id'");
        return self::getArray($res);
    }
    public static function getSingleDir($id) {
        list($id) = self::escapeArray(array($id));

        $res = self::query("SELECT * FROM galerie_dir WHERE gd_id='$id'");
        return self::getSingleRow($res);
    }
    public static function getSingleFoto($id) {
        list($id) = self::escapeArray(array($id));

        $res = self::query("SELECT * FROM galerie_foto WHERE gf_id='$id'");
        return self::getSingleRow($res);
    }
    public static function addFoto($dir, $path, $name, $kdo) {
        list($dir, $path, $name, $kdo) = self::escapeArray(array($dir, $path, $name, $kdo));

        self::query("INSERT INTO galerie_foto (gf_id_rodic,gf_path,gf_name,gf_kdo) VALUES " .
            "('$dir','$path','$name','$kdo')");
        return true;
    }
    public static function editFoto($id, $path, $dir = false, $name = false) {
        list($id, $path, $dir, $name) = DBNabidka::escapeArray(array($id, $path, $dir, $name));

        self::query("UPDATE galerie_foto SET gf_path='$path'" .
            ($dir != false ? ",gf_id_rodic='$dir'" : '') . ($name != false ? ",gf_name='$name'" : '') .
            " WHERE gf_id='$id'");

        return true;
    }
    public static function removeFoto($id) {
        list($id) = self::escapeArray(array($id));

        self::query("DELETE FROM galerie_foto WHERE gf_id='$id'");
        return true;
    }
    public static function addDir($name, $parent, $level, $hidden) {
        list($name, $parent, $level, $hidden) =
            self::escapeArray(array($name, $parent, $level, $hidden));

        self::query("INSERT INTO galerie_dir (gd_name,gd_id_rodic,gd_level,gd_hidden) VALUES " .
            "('$name','$parent','$level','$hidden')");

        return true;
    }
    public static function editDir($id, $name, $parent, $level, $hidden) {
        list($id, $name, $parent, $level, $hidden) =
            self::escapeArray(array($id, $name, $parent, $level, $hidden));

        self::query("UPDATE galerie_dir SET gd_name='$name',gd_id_rodic='$parent',
            gd_level='$level',gd_hidden='$hidden' WHERE gd_id='$id'");
    }
    public static function removeDir($id) {
        list($id) = self::escapeArray(array($id));

        self::query("DELETE FROM galerie_dir WHERE gd_id='$id'");
        self::query("DELETE FROM galerie_dir WHERE gd_id_rodic='$id'");
        self::query("DELETE FROM galerie_foto WHERE gf_id_rodic='$id'");

        return true;
    }
    public static function addFotoByPath($dir, $path, $name, $kdo) {
        list($dir, $path, $name, $kdo) = self::escapeArray(array($dir, $path, $name, $kdo));

        self::query("INSERT INTO galerie_foto (gf_id_rodic,gf_path,gf_name,gf_kdo) VALUES " .
            "((SELECT gd_id FROM (SELECT gd_id FROM galerie_dir WHERE gd_path='$dir') AS x)," .
            "'$path','$name','$kdo') ON DUPLICATE KEY UPDATE gf_name='$name'");
        return true;
    }
    public static function addDirByPath($name, $parent, $level, $path) {
        list($name, $parent, $level, $path) = self::escapeArray(array($name, $parent, $level, $path));

        self::query("INSERT INTO galerie_dir (gd_name,gd_id_rodic,gd_level,gd_path) VALUES " .
            "('$name',(SELECT gd_id FROM galerie_dir gd2 WHERE gd2.gd_path='$parent')," .
            "'$level','$path') ON DUPLICATE KEY UPDATE gd_name='$name'");

    }
    public static function removeDirByPath($path) {
        list($path) = self::escapeArray(array($path));

        self::query("DELETE FROM galerie_dir WHERE gd_id_rodic=(SELECT gd_id FROM (SELECT gd_id FROM galerie_dir WHERE gd_path='$path') AS x)");
        self::query("DELETE FROM galerie_foto WHERE gf_id_rodic=(SELECT gd_id FROM (SELECT gd_id FROM galerie_dir WHERE gd_path='$path') AS x)");
        self::query("DELETE FROM galerie_dir WHERE gd_path='$path'");
    }
    public static function removeFotoByPath($path) {
        list($path) = self::escapeArray(array($path));

        self::query("DELETE FROM galerie_foto WHERE gf_path='$path'");
        return true;
    }
}
?>