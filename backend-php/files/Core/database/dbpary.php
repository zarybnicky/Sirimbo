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

    public static function noPartner($partner)
    {
        if (!$partner) {
            return;
        }
        $dataM = self::querySingle("SELECT p_id_partnerka as u_id FROM pary WHERE p_id_partner='?' AND p_archiv='0'", $partner);
        $dataF = self::querySingle("SELECT p_id_partner as u_id FROM pary WHERE p_id_partnerka='?' AND p_archiv='0'", $partner);
        if (!$dataF) {
            $dataF = self::querySingle("SELECT p_id_partnerka as u_id FROM pary WHERE p_id_partner='?' AND p_archiv='0'", $partner);
        }
        self::query(
            "UPDATE pary SET p_archiv='1',p_timestamp_archive=NOW() WHERE (p_id_partner='?' OR p_id_partnerka='?') AND p_archiv='0'",
            $partner,
            $partner
        );
        self::query("INSERT INTO pary (p_id_partner, p_id_partnerka) VALUES ('?','0')", $partner);
        if ($dataM['u_id'] && $partner != $dataM['u_id']) {
            self::query("INSERT INTO pary (p_id_partner, p_id_partnerka) VALUES ('?','0')", $dataM['u_id']);
        }
        if ($dataF['u_id'] && $partner != $dataF['u_id'] && $dataM['u_id'] != $dataF['u_id']) {
            self::query("INSERT INTO pary (p_id_partner, p_id_partnerka) VALUES ('?','0')", $dataF['u_id']);
        }
        return self::getInsertId();
    }
}
