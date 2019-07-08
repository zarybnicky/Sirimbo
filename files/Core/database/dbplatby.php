<?php
class DBPlatby extends Database
{
    public static function checkConflicts($sid)
    {
        list($sid) = self::escape($sid);
        $res = self::query(
            "SELECT skupiny.*,platby_group.*,platby_category.*
            FROM platby_category_group pcg
                INNER JOIN platby_group_skupina ON pcg.pcg_id_group=pgs_id_group
                LEFT JOIN skupiny ON pgs_id_skupina=s_id
                LEFT JOIN platby_group ON pcg.pcg_id_group=pg_id
                LEFT JOIN platby_category ON pcg.pcg_id_category=pc_id
                INNER JOIN (
                    SELECT pcg_id FROM platby_category_group
                        INNER JOIN platby_group_skupina ON pcg_id_group=pgs_id_group
                        LEFT JOIN platby_category ON pc_id=pcg_id_category
                    WHERE pgs_id_skupina='$sid' AND pc_symbol IS NOT NULL
                    GROUP BY pc_symbol
                    HAVING COUNT(*) > 1
                ) dupl
            WHERE pcg.pcg_id=dupl.pcg_id"
        );
        return self::getArray($res);
    }

    public static function hasPaidMemberFees($uid)
    {
        list($uid) = self::escape($uid);
        $res = self::query(
            "SELECT COUNT(*) FROM platby_item
                INNER JOIN platby_category ON pc_id=pi_id_category
                INNER JOIN platby_category_group ON pcg_id_category=pc_id
                INNER JOIN platby_group ON pg_id=pcg_id_group
                INNER JOIN platby_group_skupina ON pgs_id_group=pg_id
                INNER JOIN skupiny ON pgs_id_skupina=s_id
                INNER JOIN users ON pi_id_user=u_id
            WHERE
                pg_type='1' AND
                u_id='$uid' AND
                u_skupina=s_id AND
                CURDATE() >= pc_valid_from AND
                CURDATE() <= pc_valid_to"
        );
        return self::getSingleRow($res)['COUNT(*)'];
    }

    public static function getPaymentHistory($uid)
    {
        return self::getArray(self::query(
            "SELECT * FROM platby_item
                INNER JOIN platby_category ON pi_id_category=pc_id
             WHERE pi_id_user='?'",
            $uid
        ));
    }

    public static function getOldestPayment()
    {
        $res = self::query("SELECT pi_id_user, MIN(pi_date) AS pi_date FROM platby_item GROUP BY pi_id_user");
        $out = [];
        foreach (self::getArray($res) as $row) {
            $out[$row['pi_id_user']] = $row['pi_date'];
        }
        return $out;
    }
}
