<?php
class DBPlatby extends Database
{
    public static function getInstance() { return new self(); }

    public static function checkConflicts($sid) {
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
                    WHERE pgs_id_skupina='$sid'
                    GROUP BY pc_symbol
                    HAVING COUNT(*) > 1
                ) dupl
            WHERE pcg.pcg_id=dupl.pcg_id"
        );
        return self::getArray($res);
    }

    public static function hasPaidMemberFees($uid) {
        list($uid) = self::escape($uid);
        $res = self::query(
            "SELECT * FROM platby_item
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
                (CURDATE() >= pc_valid_from OR CURDATE() <= pc_valid_to)"
        );
        return self::getArray($res);
    }
}
?>