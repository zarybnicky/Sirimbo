<?php
class DBRozpis extends Database
{
    public static function getSchedules($descending = false)
    {
        $res = self::query(
            "SELECT u_jmeno,u_prijmeni,r_id,r_trener,r_kde,r_datum,r_visible,r_lock" .
            " FROM rozpis LEFT JOIN users ON r_trener=u_id ORDER BY r_datum" .
            ($descending ? ' DESC' : '')
        );
        return self::getArray($res);
    }

    public static function getSchedulesByTrainer($trener, $descending = false)
    {
        $res = self::query(
            "SELECT u_jmeno,u_prijmeni,r_id,r_trener,r_kde,r_datum,r_visible,r_lock
            FROM rozpis LEFT JOIN users ON r_trener=u_id
            WHERE r_trener='?'
            ORDER BY r_datum" . ($descending ? ' DESC' : ''),
            $trener,
        );
        return self::getArray($res);
    }

    public static function getSchedule($id)
    {
        $res = self::query(
            "SELECT r_id,r_trener,u_jmeno,u_prijmeni,r_kde,r_datum,r_visible,r_lock" .
            " FROM rozpis LEFT JOIN users ON r_trener=u_id WHERE r_id='?'",
            $id,
        );
        return $res ? self::getSingleRow($res) : false;
    }

    public static function reserveLesson($rid, $uid)
    {
        if (!self::isLessonFree($rid)) {
            return false;
        }
        self::query("UPDATE rozpis_item SET ri_partner='?' WHERE ri_id='?'", $uid, $rid);
        return true;
    }

    public static function cancelLesson($rid)
    {
        if (self::isLessonFree($rid)) {
            return false;
        }
        self::query("UPDATE rozpis_item SET ri_partner=NULL WHERE ri_id='?'", $rid);
        return true;
    }

    public static function getLessons($rid)
    {
        $res = self::query(
            "SELECT p_id,u_id,u_login,u_jmeno,u_prijmeni,ri_id,ri_id_rodic,ri_partner,
               ri_od,ri_do,ri_lock
            FROM rozpis_item
                LEFT JOIN pary ON ri_partner=p_id
                LEFT JOIN users ON p_id_partner=u_id
            WHERE ri_id_rodic='?'
            ORDER BY ri_od",
            $rid,
        );
        return self::getArray($res);
    }

    public static function getLesson($ri_id)
    {
        $res = self::query(
            "SELECT u_id,u_login,u_jmeno,u_prijmeni,r_trener,ri_id,ri_id_rodic,ri_partner," .
            "ri_od,ri_do,ri_lock FROM rozpis_item LEFT JOIN users ON ri_partner=u_id" .
            " LEFT JOIN rozpis ON ri_id_rodic=r_id WHERE ri_id='?'",
            $ri_id,
        );
        return self::getSingleRow($res);
    }

    public static function isLessonFree($rid)
    {
        $res = self::query("SELECT ri_partner FROM rozpis_item WHERE ri_id='?'", $rid);
        if (!$res) {
            return false;
        }
        $row = self::getSingleRow($res);
        return $row["ri_partner"] === null;
    }

    public static function getScheduleTrainer($id)
    {
        $res = self::query(
            "SELECT * FROM users
            WHERE u_id=(SELECT r_trener FROM rozpis WHERE r_id='?')",
            $id
        );
        return $res ? self::getSingleRow($res) : false;
    }

    public static function addSchedule($trener, $kde, $datum, $visible, $lock)
    {
        self::query(
            "INSERT INTO rozpis (r_trener,r_kde,r_datum,r_visible,r_lock) VALUES " .
            "('?','?','?','?','?')",
            $trener,
            $kde,
            $datum,
            $visible,
            $lock,
        );
        return self::getInsertId();
    }

    public static function editSchedule($id, $trener, $kde, $datum, $visible, $lock)
    {
        self::query(
            "UPDATE rozpis SET r_trener='?',r_kde='?',r_datum='?'," .
            "r_visible='?',r_lock='?' WHERE r_id='?'",
            $trener,
            $kde,
            $datum,
            $visible,
            $lock,
            $id,
        );
        return true;
    }

    public static function deleteSchedule($id)
    {
        self::query("DELETE FROM rozpis WHERE r_id='?'", $id);
        self::query("DELETE FROM rozpis_item WHERE ri_id_rodic='?'", $id);
        return true;
    }

    public static function addLesson($parent_id, $user_id, $od, $do, $lock)
    {
        $user_id = $user_id ? "'$user_id'" : "NULL";
        self::query(
            "INSERT INTO rozpis_item (ri_id_rodic,ri_partner,ri_od,ri_do,ri_lock)" .
            " VALUES ('?',$user_id,'?','?','?')" .
            " ON DUPLICATE KEY UPDATE ri_partner=$user_id,ri_do='?',ri_lock='?'",
            $parent_id,
            $od,
            $do,
            $lock,
            $do,
            $lock,
        );
        return self::getInsertId();
    }

    public static function editMultipleLessons($data)
    {
        $ids = array_map(fn($item) => $item['ri_id'], $data);
        $data = array_map(
            fn($item) => array_intersect_key(
                $item,
                array_flip(['ri_partner', 'ri_od', 'ri_do', 'ri_lock'])
            ),
            $data
        );

        $columns = array_keys(reset($data));
        $rows = self::escapeArray(array_values($data));

        $q = 'UPDATE rozpis_item SET ';

        $columns_string = [];
        foreach ($columns as $col_index => $col) {
            $s = $col . ' = CASE';
            foreach ($rows as $row_index => $row) {
                $value = $row[$col_index] ? "'{$row[$col_index]}'" : "'0'" ;
                if ($col == 'ri_partner' && $value == "'0'") {
                    $value = 'NULL';
                }
                $s .= ' WHEN ri_id="' . $ids[$row_index] . '" THEN ' . $value  . '';
            }
            $s .= ' ELSE ' . $col . ' END';
            $columns_string[] = $s;
        }

        $q .= implode(', ', $columns_string);
        $q .= ' WHERE ri_id IN ("' . implode('","', $ids) . '")';

        self::query($q);

        return $ids;
    }

    public static function deleteLesson($id)
    {
        self::query("DELETE FROM rozpis_item WHERE ri_id='?'", $id);
        return true;
    }
}
