<?php
class DBNabidka extends Database
{
    public static function addNabidka($trener, $pocet_hod, $max_hod, $od, $do, $visible, $lock)
    {
        \Database::query(
            "INSERT INTO nabidka
            (n_trener,n_pocet_hod,n_max_pocet_hod,n_od,n_do,n_visible,n_lock) VALUES
            ('?','?','?','?','?','?','?')",
            $trener, $pocet_hod, $max_hod, $od, $do, $visible, $lock
        );
    }

    public static function getReservationItems($parent_id)
    {
        return \Database::queryArray(
            "SELECT p_id,u_id,u_jmeno,u_prijmeni,nabidka_item.*
            FROM nabidka_item LEFT JOIN pary ON ni_partner=p_id LEFT JOIN users ON p_id_partner=u_id
            WHERE ni_id_rodic='?'",
            $parent_id
        );
    }

    public static function addNabidkaItemLessons($user_id, $parent_id, $pocet_hod)
    {
        \Database::query(
            "INSERT INTO nabidka_item (ni_partner,ni_id_rodic,ni_pocet_hod)" .
            " VALUES ('?','?','?') ON CONFLICT (ni_id_rodic, ni_partner)" .
            " DO UPDATE SET ni_pocet_hod=nabidka_item.ni_pocet_hod+EXCLUDED.ni_pocet_hod",
            $user_id,
            $parent_id,
            $pocet_hod
        );
    }

    public static function editNabidkaItem($id, $partner, $pocet_hod)
    {
        $row = \Database::querySingle(
            "SELECT ni_id,ni_id_rodic FROM nabidka_item
            WHERE ni_partner='?' AND ni_id_rodic=(SELECT ni_id_rodic FROM nabidka_item WHERE ni_id='?')",
            $partner,
            $id,
        );
        if (!$row) {
            return false;
        }
        if ($row['ni_id'] && $row['ni_id'] != $id) { //If there is a conflicting nabidka
            \Database::query("DELETE FROM nabidka_item WHERE ni_id='?'", $id);
            \DBNabidka::addNabidkaItemLessons($partner, $row['ni_id_rodic'], $pocet_hod);
        } else {
            \Database::query(
                "UPDATE nabidka_item SET ni_partner='?', ni_pocet_hod='?' WHERE ni_id='?'",
                $partner,
                $pocet_hod,
                $id,
            );
        }
    }
}
