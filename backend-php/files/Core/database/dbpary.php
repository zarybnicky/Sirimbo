
<?php
class DBPary extends Database
{
    public static function getActivePary()
    {
        return self::queryArray(
            "SELECT p_id,
                m.u_id AS man,m.u_id AS man_id,m.u_jmeno AS man_name,m.u_prijmeni AS man_surname,
                f.u_id AS woman,f.u_id AS woman_id,f.u_jmeno AS woman_name,f.u_prijmeni AS woman_surname
            FROM pary AS p
                LEFT JOIN users AS m ON p.p_id_partner=m.u_id
                LEFT JOIN users AS f ON p.p_id_partnerka=f.u_id
            WHERE p.p_archiv='0'
                AND p.p_id_partner!='0' AND p.p_id_partnerka!='0'
                AND m.u_id IS NOT NULL AND f.u_id IS NOT NULL
            ORDER BY man_surname ASC"
        );
    }

    public static function getSinglePar($id)
    {
        return self::querySingle(
            "SELECT p_id,
                m.u_id AS man_id,m.u_jmeno AS man_name,m.u_prijmeni AS man_surname,
                f.u_id AS woman_id,f.u_jmeno AS woman_name,f.u_prijmeni AS woman_surname
            FROM pary AS p
                LEFT JOIN users AS m ON p.p_id_partner=m.u_id
                LEFT JOIN users AS f ON p.p_id_partnerka=f.u_id
            WHERE p.p_id='?'",
            $id
        );
    }

    public static function getLatestPartner($partner, $pohlavi)
    {
        if ($pohlavi == 'm') {
            $res = self::querySingle(
                "SELECT * FROM pary LEFT JOIN users ON p_id_partnerka=u_id " .
                "WHERE p_id_partner='?' AND p_archiv='0'",
                $partner
            );
        } else {
            $res = self::querySingle(
                "SELECT * FROM pary LEFT JOIN users ON p_id_partner=u_id " .
                "WHERE p_id_partnerka!='0' AND p_id_partnerka='?' AND p_archiv='0'",
                $partner
            );
        }
        if (!$res && $pohlavi == 'f') {
            $res = self::querySingle(
                "SELECT *
                FROM pary LEFT JOIN users ON p_id_partnerka=u_id
                WHERE p_id_partner='$partner' AND p_archiv='0'"
            );
        }
        return $res;
    }

    public static function getPartners($currentCouples = [])
    {
        $currentCouples = array_filter($currentCouples);
        $couples = $currentCouples
            ? (" OR p_id IN (" . implode(',', $currentCouples) . ")")
            : '';
        return self::queryArray(
            "SELECT * FROM pary LEFT JOIN users ON p_id_partner=u_id
            WHERE p_archiv='0' $couples ORDER BY u_prijmeni"
        );
    }

    public static function removeCouple($id)
    {
        $data = self::getSinglePar($id);
        self::noPartner($data['man_id']);
    }

    public static function noPartner($partner)
    {
        if ($partner == '0') {
            return;
        }
        $dataM = self::getLatestPartner($partner, 'm');
        $dataF = self::getLatestPartner($partner, 'f');
        self::query(
            "UPDATE pary SET p_archiv='1',p_timestamp_archive=NOW()" .
            " WHERE (p_id_partner='?' OR p_id_partnerka='?') AND p_archiv='0'",
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
