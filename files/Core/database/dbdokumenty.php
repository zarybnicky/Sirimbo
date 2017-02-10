<?php
class DBDokumenty extends Database
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
        list($ids) = self::escape($ids);
        if (empty($ids)) {
            return array();
        }

        $query = 'SELECT *
            FROM dokumenty
            LEFT JOIN users ON d_kdo=u_id
            WHERE d_id IN (\'' . implode('\',\'', $ids) . '\')';

        $res = self::query($query);
        return self::getArray($res);
    }

    public static function getDokumentyByKategorie($kat)
    {
        $res = self::query(
            "SELECT *
            FROM dokumenty
            LEFT JOIN users ON d_kdo=u_id
            WHERE d_kategorie='?'
            ORDER BY d_id DESC",
            $kat
        );
        return self::getArray($res);
    }

    public static function getDokumentyByAuthor($author)
    {
        $res = self::query(
            "SELECT *
            FROM dokumenty
            LEFT JOIN users ON d_kdo=u_id
            WHERE d_kdo='?'
            ORDER BY d_id DESC",
            $author
        );
        return self::getArray($res);
    }

    public static function getSingleDokument($id)
    {
        $res = self::query(
            "SELECT *
            FROM dokumenty
            LEFT JOIN users ON d_kdo=u_id
            WHERE d_id='?'",
            $id
        );
        if (!$res) {
            return false;
        } else {
            return self::getSingleRow($res);
        }
    }

    public static function getDokumentPath($id)
    {
        $res = self::query("SELECT d_path FROM dokumenty WHERE d_id='?'", $id);
        if (!$res) {
            return false;
        } else {
            $row = self::getSingleRow($res);
            return $row["d_path"];
        }
    }

    public static function getDokumentUserID($id)
    {
        $res = self::query("SELECT d_kdo FROM dokumenty WHERE d_id='?'", $id);
        if (!$res) {
            return false;
        } else {
            $row = self::getSingleRow($res);
            return $row["d_kdo"];
        }
    }

    public static function getDokumentName($id)
    {
        $res = self::query("SELECT d_name FROM dokumenty WHERE d_id='?'", $id);
        if (!$res) {
            return false;
        } else {
            $row = self::getSingleRow($res);
            return $row["d_name"];
        }
    }

    public static function addDokument($path, $name, $filename, $kategorie, $kdo)
    {
        self::query(
            "INSERT INTO dokumenty (d_path,d_name,d_filename,d_kategorie,d_kdo) VALUES " .
            "('?','?','?','?','?')",
            $path, $name, $filename, $kategorie, $kdo
        );
        return self::getInsertId();
    }

    public static function editDokument($id, $newname)
    {
        self::query(
            "UPDATE dokumenty SET d_name='?' WHERE d_id='?'",
            $newname, $id
        );
        return true;
    }

    public static function removeDokument($id)
    {
        self::query("DELETE FROM dokumenty WHERE d_id='?'", $id);
        return true;
    }
}
