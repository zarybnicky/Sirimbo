<?php
class DBDokumenty extends Database
{
    public static function getMultipleById($ids)
    {
        if (!array_filter($ids)) {
            return [];
        }
        $res = self::query(
            'SELECT * FROM dokumenty
            LEFT JOIN users ON d_kdo=u_id
            WHERE d_id IN (\'' . implode('\',\'', array_filter($ids)) . '\')'
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
}
