<?php
class DBRozpis extends Database
{
    public static function getLessons($rid)
    {
        return \Database::queryArray(
            "SELECT p_id,u_id,u_login,u_jmeno,u_prijmeni,ri_id,ri_id_rodic,ri_partner,ri_od,ri_do,ri_lock
            FROM rozpis_item
                LEFT JOIN pary ON ri_partner=p_id
                LEFT JOIN users ON p_id_partner=u_id
            WHERE ri_id_rodic='?'
            ORDER BY ri_od",
            $rid,
        );
    }

    public static function addLesson($parent_id, $user_id, $od, $do, $lock)
    {
        $user_id = $user_id ? "'$user_id'" : "NULL";
        \Database::query(
            "INSERT INTO rozpis_item (ri_id_rodic,ri_partner,ri_od,ri_do,ri_lock) VALUES ('?',$user_id,'?','?','?')",
            $parent_id,
            $od,
            $do,
            $lock,
        );
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
        $rows = \Database::escapeArray(array_values($data));

        $q = 'UPDATE rozpis_item SET ';

        $columns_string = [];
        foreach ($columns as $col_index => $col) {
            $s = $col . ' = CASE';
            foreach ($rows as $row_index => $row) {
                $value = $row[$col_index] ? "'{$row[$col_index]}'" : "'0'" ;
                if ($col == 'ri_partner' && $value == "'0'") {
                    $value = 'NULL';
                }
                $s .= " WHEN ri_id='" . $ids[$row_index] . "' THEN " . $value;
            }
            $s .= ' ELSE ' . $col . ' END';
            $columns_string[] = $s;
        }

        $q .= implode(', ', $columns_string);
        $q .= " WHERE ri_id IN ('" . implode("','", $ids) . "')";

        \Database::query($q);
    }
}
