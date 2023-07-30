<?php
class DBPary extends Database
{
    public static function getPartners($currentCouples = [])
    {
        $couples = $currentCouples ? (" OR p_id IN (" . implode(',', $currentCouples) . ")") : '';
        return self::queryArray(
            "SELECT * FROM pary LEFT JOIN users ON p_id_partner=u_id
            WHERE p_archiv='0' $couples ORDER BY u_prijmeni"
        );
    }
}
