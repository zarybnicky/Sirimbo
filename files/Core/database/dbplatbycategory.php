<?php
class DBPlatbyCategory extends Database
{
    public static function insert($name, $symbol, $amount, $dueDate, $validFrom, $validTo, $useBase, $usePrefix, $archive) {
        list($name, $symbol, $amount, $dueDate, $validFrom, $validTo, $useBase, $usePrefix, $archive) =
            self::escape($name, $symbol, $amount, $dueDate, $validFrom, $validTo, $useBase, $usePrefix, $archive);
        self::query(
                "INSERT INTO platby_category
                    (pc_name,pc_symbol,pc_amount,pc_date_due,pc_valid_from,pc_valid_to,pc_use_base,pc_use_prefix,pc_archive)
                VALUES
                    ('$name',$symbol,'$amount','$dueDate','$validFrom','$validTo','$useBase','$usePrefix','$archive')"
        );
    }
    public static function update($id, $name, $symbol, $amount, $dueDate, $validFrom, $validTo, $useBase, $usePrefix, $archive) {
        list($id, $name, $symbol, $amount, $dueDate, $validFrom, $validTo, $useBase, $usePrefix, $archive) =
            self::escape($id, $name, $symbol, $amount, $dueDate, $validFrom, $validTo, $useBase, $usePrefix, $archive);
        self::query(
                "UPDATE platby_category SET
                    pc_name='$name',pc_symbol=$symbol,pc_amount='$amount',pc_date_due='$dueDate',pc_valid_from='$validFrom',
                    pc_valid_to='$validTo',pc_use_base='$useBase',pc_use_prefix='$usePrefix',pc_archive='$archive'
                WHERE pc_id='$id'"
        );
    }
    public static function delete($id) {
        list($id) = self::escape($id);
        self::query(
                "DELETE FROM platby_category
                WHERE pc_id='$id'"
        );
    }
    public static function checkActiveSymbol($symbol) {
        list($symbol) = self::escape($symbol);

        $res = self::query(
                "SELECT * FROM platby_category
                WHERE pc_archive='0' AND pc_symbol='$symbol'"
        );
        if ($res)
            return self::getSingleRow($res);
        else
            return false;
    }
    public static function get($archived = null) {
        $res = self::query('SELECT * FROM platby_category' .
                ($archived !== null ? (" WHERE pc_archive='" . ($archived ? '1' : '0') . "'") : '') . ' ORDER BY pc_symbol');
        return self::getArray($res);
    }
    public static function getSingle($id) {
        list($id) = self::escape($id);
        $res = self::query("SELECT * FROM platby_category WHERE pc_id='$id'");
        return self::getSingleRow($res);
    }
    public static function getOrphan() {
        $res = self::query(
                'SELECT * FROM platby_category
                WHERE NOT EXISTS (
                    SELECT pcg_id FROM platby_category_group WHERE pcg_id_category=pc_id
                )');
        return self::getArray($res);
    }
    public static function getNotInGroup($id) {
        list($id) = self::escape($id);
        $res = self::query(
                "SELECT * FROM platby_category
                WHERE NOT EXISTS (
                    SELECT pcg_id FROM platby_category_group WHERE pcg_id_category=pc_id AND pcg_id_group='$id'
                )");
        return self::getArray($res);
    }
    public static function getSingleWithGroups($id) {
        list($id) = self::escape($id);
        $res = self::query(
                "SELECT *
                FROM platby_category_group
                    LEFT JOIN platby_group ON pcg_id_group=pg_id
                    LEFT JOIN platby_category ON pcg_id_category=pc_id
                WHERE pc_id='$id'
                ORDER BY pg_type,pg_id,pc_symbol"
        );
        return self::getArray($res);
    }
}