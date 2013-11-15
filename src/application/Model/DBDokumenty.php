<?php
namespace TKOlomouc\Model;

use TKOlomouc\Model\Database\Adapter;

class DBDokumenty extends Adapter
{
    public static function getDokumenty()
    {
        $res = self::query(
            "SELECT *
            FROM dokumenty
            LEFT JOIN users ON d_kdo=u_id
            ORDER BY d_id DESC"
        );
        return self::getArray($res);
    }
    public static function getMultipleById($ids)
    {
        list($kat) = self::escape($ids);

        $query = 'SELECT *
            FROM dokumenty
            LEFT JOIN users ON d_kdo=u_id
            WHERE d_id IN (';
        for($i = 0; $i < count($ids); $i++) {
            $query .= "'{$ids[$i]}'";
            if ($i + 1 < count($ids)) {
                $query .= ',';
            }
        }
        $query .= ')';
        $res = self::query($query);
        return self::getArray($res);
    }
    public static function getDokumentyByKategorie($kat)
    {
        list($kat) = self::escape($kat);
        $res = self::query(
            "SELECT *
            FROM dokumenty
            LEFT JOIN users ON d_kdo=u_id
            WHERE d_kategorie='$kat'
            ORDER BY d_id DESC"
        );
        return self::getArray($res);
    }
    public static function getSingleDokument($id)
    {
        list($id) = self::escape($id);
        $res = self::query(
            "SELECT *
            FROM dokumenty
            LEFT JOIN users ON d_kdo=u_id
            WHERE d_id='$id'"
        );
        if (!$res) {
            return false;
        } else {
            return self::getSingleRow($res);
        }
    }
    public static function addDokument($path, $name, $filename, $kategorie, $kdo)
    {
        list($path, $name, $filename, $kategorie, $kdo) =
            self::escape($path, $name, $filename, $kategorie, $kdo);

        self::query("INSERT INTO dokumenty (d_path,d_name,d_filename,d_kategorie,d_kdo) VALUES " .
            "('$path','$name','$filename','$kategorie','$kdo')");

        return self::getInsertId();
    }
    public static function editDokument($id, $name, $category)
    {
        list($id, $name, $category) = self::escape($id, $name, $category);

        self::query("UPDATE dokumenty SET d_kategorie='$category',d_name='$ame' WHERE d_id='$id'");

        return true;
    }
    public static function removeDokument($id) {
        list($id) = self::escape($id);

        self::query("DELETE FROM dokumenty WHERE d_id='$id'");

        return true;
    }
}
?>