<?php
class DBPlatbyItem extends Database
{
    public static function insert($uid, $cid, $rid, $amount, $date, $prefix)
    {
        self::query(
            "INSERT INTO platby_item
                (pi_id_user,pi_id_category,pi_id_raw,pi_amount,pi_date,pi_prefix)
            VALUES
                ('?','?'," . ($rid !== '' ? "'$rid'" : "NULL") . ",'?','?','?')
            ON CONFLICT (pi_id_raw) DO UPDATE SET
                pi_id_user=EXCLUDED.pi_id_user,
                pi_id_category=EXCLUDED.pi_id_category,
                pi_amount=EXCLUDED.pi_amount,
                pi_date=EXCLUDED.pi_date,
                pi_prefix=EXCLUDED.pi_prefix",
            $uid,
            $cid,
            $amount,
            $date,
            $prefix,
        );
    }

    public static function update($id, $uid, $cid, $amount, $date, $prefix)
    {
        self::query(
            "UPDATE platby_item
            SET pi_id_user='?', pi_id_category='?', pi_amount='?', pi_date='?', pi_prefix='?'
            WHERE pi_id='?'",
            $uid,
            $cid,
            $amount,
            $date,
            $prefix,
            $id,
        );
    }

    public static function remove($id)
    {
        self::query("DELETE FROM platby_item WHERE pi_id='?'", $id);
    }

    public static function get($joined = false, $filter = [], $date = [])
    {
        $query
            = 'SELECT * FROM (SELECT DISTINCT ON (pi_id) * FROM platby_item'
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
            if (!empty($date['from']) && $date['from']->isValid()) {
                $query .= ' AND pi_date >= "' . $date['from']->getDate() . '" ';
            }
            if (!empty($date['to']) && $date['to']->isValid()) {
                $query .= ' AND pi_date <= "' . $date['to']->getDate() . '" ';
            }
        }

        $query .= ') t ORDER BY t.pi_date DESC';

        $res = self::query($query);
        return self::getArray($res);
    }

    public static function getSingle($id, $joined = false)
    {
        $res = self::query(
            "SELECT * FROM platby_item"
            . ($joined ?
               ' LEFT JOIN users ON pi_id_user=u_id
                 LEFT JOIN platby_category ON pi_id_category=pc_id' : '')
            . " WHERE pi_id='?'",
            $id
        );
        return self::getSingleRow($res);
    }

    public static function getSingleByRawId($id)
    {
        $res = self::query("SELECT * FROM platby_item WHERE pi_id_raw='?'", $id);
        return self::getSingleRow($res);
    }
}