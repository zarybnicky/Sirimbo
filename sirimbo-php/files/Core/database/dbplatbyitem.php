<?php
class DBPlatbyItem extends Database
{
    public static function insert($uid, $cid, $rid, $amount, $date, $prefix)
    {
        list($uid, $cid, $rid, $amount, $date, $prefix)
            = self::escape($uid, $cid, $rid, $amount, $date, $prefix);

        self::query(
            "INSERT INTO platby_item
                (pi_id_user,pi_id_category,pi_id_raw,pi_amount,pi_date,pi_prefix)
            VALUES
                ('$uid','$cid'," . ($rid !== '' ? "'$rid'" : "NULL") . ",'$amount','$date','$prefix')
            ON DUPLICATE KEY UPDATE
                pi_id_user=VALUES(pi_id_user),
                pi_id_category=VALUES(pi_id_category),
                pi_amount=VALUES(pi_amount),
                pi_date=VALUES(pi_date),
                pi_prefix=VALUES(pi_prefix)"
        );
    }

    public static function update($id, $uid, $cid, $amount, $date, $prefix)
    {
        list($id, $uid, $cid, $amount, $date, $prefix)
            = self::escape($id, $uid, $cid, $amount, $date, $prefix);

        self::query(
            "UPDATE platby_item SET
                pi_id_user='$uid',
                pi_id_category='$cid',
                pi_amount='$amount',
                pi_date='$date',
                pi_prefix='$prefix'
            WHERE pi_id='$id'"
        );
    }

    public static function remove($id)
    {
        list($id) = self::escape($id);
        self::query("DELETE FROM platby_item WHERE pi_id='$id'");
    }

    public static function get($joined = false, $filter = [], $sort = ['pi_date DESC'], $date = [])
    {
        $query
            = 'SELECT * FROM platby_item'
            . ($joined ?
               ' LEFT JOIN users ON pi_id_user=u_id
                 LEFT JOIN platby_category ON pi_id_category=pc_id
                 LEFT JOIN platby_category_group ON pcg_id_category=pc_id
                 LEFT JOIN platby_group ON pg_id=pcg_id_group' : '');

        if ($filter) {
            $filter = array_combine(array_keys($filter), self::escapeArray(array_values($filter)));
            $where = [];
            foreach ($filter ? $filter : [] as $key => $value) {
                if (!is_array($value)) {
                    $where[] = "$key = '$value'";
                } else {
                    $where[] = "$key IN ('" . implode("','", $value) . "')";
                }
            }
            $query .= ' WHERE ' . implode(' AND ', $where);
        }
        if (!empty($date)) {
            if (strpos($query, 'WHERE') === false) {
                $query .= ' WHERE 1=1 ';
            }
            if ($date['from']->isValid()) {
                $query .= ' AND pi_date >= "' . $date['from']->getDate() . '" ';
            }
            if ($date['to']->isValid()) {
                $query .= ' AND pi_date <= "' . $date['to']->getDate() . '" ';
            }
        }

        $query .= ' GROUP BY pi_id';

        if (!empty($sort)) {
            $query .= ' ORDER BY ' . implode(', ', $sort);
        }

        $res = self::query($query);
        return self::getArray($res);
    }

    public static function getSingle($id, $joined = false)
    {
        list($id) = self::escape($id);

        $res = self::query(
            "SELECT * FROM platby_item"
            . ($joined ?
               ' LEFT JOIN users ON pi_id_user=u_id
                 LEFT JOIN platby_category ON pi_id_category=pc_id' : '')
            . " WHERE pi_id='$id'"
        );
        return self::getSingleRow($res);
    }

    public static function getSingleByRawId($id)
    {
        list($id) = self::escape($id);

        $res = self::query("SELECT * FROM platby_item WHERE pi_id_raw='$id'");
        return self::getSingleRow($res);
    }
}