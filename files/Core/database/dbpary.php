<?php
class DBPary extends Database
{
    public static function getPary()
    {
        $res = self::query("SELECT * FROM pary ORDER BY p_timestamp_add ASC");
        return self::getArray($res);
    }

    public static function getActivePary()
    {
        $res = self::query(
            "SELECT p_id,
                m.u_id AS guy,m.u_id AS guy_id,m.u_jmeno AS guy_name,m.u_prijmeni AS guy_surname,
                f.u_id AS gal,f.u_id AS gal_id,f.u_jmeno AS gal_name,f.u_prijmeni AS gal_surname,
                p_stt_trida,p_stt_body,p_stt_finale,
                p_lat_trida,p_lat_body,p_lat_finale,p_hodnoceni
            FROM pary AS p
                LEFT JOIN users AS m ON p.p_id_partner=m.u_id
                LEFT JOIN users AS f ON p.p_id_partnerka=f.u_id
            WHERE p.p_archiv='0'
                AND p.p_id_partner!='0' AND p.p_id_partnerka!='0'
                AND m.u_id IS NOT NULL AND f.u_id IS NOT NULL
            ORDER BY guy_surname ASC"
        );

        return self::getArray($res);
    }

    public static function getActiveParyByHodnoceni()
    {
        $res = self::query(
            "SELECT p_id,
                m.u_id AS guy,m.u_id AS guy_id,m.u_jmeno AS guy_name,m.u_prijmeni AS guy_surname,
                f.u_id AS gal,f.u_id AS gal_id,f.u_jmeno AS gal_name,f.u_prijmeni AS gal_surname,
                p_stt_trida,p_stt_body,p_stt_finale,
                p_lat_trida,p_lat_body,p_lat_finale,p_hodnoceni
            FROM pary AS p
                LEFT JOIN users AS m ON p.p_id_partner=m.u_id
                LEFT JOIN users AS f ON p.p_id_partnerka=f.u_id
            WHERE p.p_archiv='0' AND (f.u_jmeno IS NOT NULL OR f.u_prijmeni IS NOT NULL)
            ORDER BY p.p_hodnoceni DESC"
        );
        return self::getArray($res);
    }

    public static function getSinglePar($id)
    {
        $res = self::query(
            "SELECT p_id,
                m.u_id AS guy_id,m.u_jmeno AS guy_name,m.u_prijmeni AS guy_surname,
                f.u_id AS gal_id,f.u_jmeno AS gal_name,f.u_prijmeni AS gal_surname,
                p_stt_trida,p_stt_body,p_stt_finale,
                p_lat_trida,p_lat_body,p_lat_finale,p_hodnoceni
            FROM pary AS p
                LEFT JOIN users AS m ON p.p_id_partner=m.u_id
                LEFT JOIN users AS f ON p.p_id_partnerka=f.u_id
            WHERE p.p_id='?'",
            $id
        );

        return $res ? self::getSingleRow($res) : false;
    }

    public static function getLatestPartner($partner, $pohlavi)
    {
        list($partner, $pohlavi) = self::escape($partner, $pohlavi);

        if ($pohlavi == 'm') {
            $q = 'SELECT * FROM pary'
                . ' LEFT JOIN users ON p_id_partnerka=u_id'
                . ' WHERE p_id_partner="' . $partner . '"'
                . ' AND p_archiv="0"';
        } else {
            $q = 'SELECT * FROM pary'
                . ' LEFT JOIN users ON p_id_partner=u_id'
                . ' WHERE p_id_partnerka!="0"'
                . ' AND p_id_partnerka="' . $partner . '"'
                . ' AND p_archiv="0"';
        }
        $res = self::query($q);
        if (($res === false || ($res = self::getSingleRow($res)) == []) && $pohlavi == 'f') {
            $res = self::query(
                "SELECT *
                FROM pary
                    LEFT JOIN users ON p_id_partnerka=u_id
                WHERE p_id_partner='$partner' AND p_archiv='0'"
            );
            if ($res !== false) {
                $res = self::getSingleRow($res);
            }
        }
        return $res;
    }

    public static function getUnpairedUsers()
    {
        $res = self::query(
            'SELECT u_jmeno,u_prijmeni,u_id
            FROM users
            WHERE u_id NOT IN
            (SELECT u_id FROM users
               LEFT JOIN pary ON p_id_partnerka=u_id OR p_id_partner=u_id
             WHERE p_archiv="0")'
        );
        return self::getArray($res);
    }

    public static function getPreviousPartners($id)
    {
        $res = self::query(
            "SELECT p_id,
                m.u_id AS guy_id,m.u_jmeno AS guy_name,m.u_prijmeni AS guy_surname,
                f.u_id AS gal_id,f.u_jmeno AS gal_name,f.u_prijmeni AS gal_surname,
                p_stt_trida,p_stt_body,p_stt_finale,
                p_lat_trida,p_lat_body,p_lat_finale,p_hodnoceni
            FROM pary AS p
                LEFT JOIN users AS m ON p.p_id_partner=m.u_id
                LEFT JOIN users AS f ON p.p_id_partnerka=f.u_id
            WHERE (p.p_id_partner='?' OR p.p_id_partnerka='?') AND p.p_archiv='1'",
            $id, $id
        );
        return self::getArray($res);
    }

    public static function getPartners()
    {
        $res = self::query(
            "SELECT * FROM pary LEFT JOIN users ON p_id_partner=u_id
            WHERE p_archiv='0' ORDER BY u_prijmeni"
        );
        return self::getArray($res);
    }

    public static function newCouple($partner, $partnerka)
    {
        list($partner, $partnerka) = self::escape($partner, $partnerka);
        if ($partner == '0' || $partner == '0') {
            if ($partner != '0') {
                self::noPartner($partner);
            }
            if ($partnerka != '0') {
                self::noPartner($partnerka);
            }
            return;
        }

        $oldM = self::getLatestPartner($partner, 'm');
        $oldF = self::getLatestPartner($partnerka, 'f');

        self::query(
            "UPDATE pary
            SET p_archiv='1',p_timestamp_archive=NOW()
            WHERE (p_id_partner='$partner' OR p_id_partner='$partnerka' OR
                p_id_partnerka='$partnerka') AND p_archiv='0'"
        );

        if ($oldM['u_id']) {
            self::noPartner($oldM['u_id']);
        }
        if ($oldF['u_id']) {
            self::noPartner($oldF['u_id']);
        }

        self::query("INSERT INTO pary (p_id_partner, p_id_partnerka) VALUES ('$partner','$partnerka')");
    }

    public static function removeCouple($id)
    {
        $data = self::getSinglePar($id);
        self::noPartner($data['guy_id']);
    }

    public static function noPartner($partner)
    {
        list($partner) = self::escape($partner);
        if ($partner == '0') {
            return;
        }
        $dataM = self::getLatestPartner($partner, 'm');
        $dataF = self::getLatestPartner($partner, 'f');

        self::query("UPDATE pary SET p_archiv='1',p_timestamp_archive=NOW()" .
            " WHERE (p_id_partner='$partner' OR p_id_partnerka='$partner') AND p_archiv='0'");

        self::query("INSERT INTO pary (p_id_partner, p_id_partnerka) VALUES ('$partner','0')");
        if ($dataM['u_id'] && $partner != $dataM['u_id']) {
            self::query("INSERT INTO pary (p_id_partner, p_id_partnerka) VALUES ('{$dataM['u_id']}','0')");
        }
        if ($dataF['u_id'] && $partner != $dataF['u_id'] && $dataM['u_id'] != $dataF['u_id']) {
            self::query("INSERT INTO pary (p_id_partner, p_id_partnerka) VALUES ('{$dataF['u_id']}','0')");
        }
        return self::getInsertId();
    }

    public static function getPartnerRequestsForMe($id)
    {
        list($id) = self::escape($id);

        $res = self::query(
            "SELECT pn_id, pn_navrhl, u_jmeno, u_prijmeni, u_pohlavi
            FROM pary_navrh LEFT JOIN users ON pn_navrhl=u_id
            WHERE (pn_partner='$id' OR pn_partnerka='$id') AND pn_navrhl!='$id'"
        );

        return self::getArray($res);
    }

    public static function getPartnerRequestsByMe($id)
    {
        list($id) = self::escape($id);

        if (Session::getUserPohlavi() == "m") {
            $res = self::query(
                "SELECT pn_id, u_id, u_jmeno, u_prijmeni, u_pohlavi
                FROM pary_navrh LEFT JOIN users ON pn_partnerka=u_id
                WHERE pn_partner='$id' AND pn_navrhl='$id'"
            );
        } else {
            $res = self::query(
                "SELECT pn_id, u_id, u_jmeno, u_prijmeni, u_pohlavi
                FROM pary_navrh LEFT JOIN users ON pn_partner=u_id
                WHERE pn_partnerka='$id' AND pn_navrhl='$id'"
            );
        }
        return self::getArray($res);
    }

    public static function newPartnerRequest($navrhl, $partner, $partnerka)
    {
        self::query(
            "INSERT INTO pary_navrh
            (pn_navrhl, pn_partner, pn_partnerka)
            VALUES (?, ?, ?)",
            $navrhl, $partner, $partnerka
        );
    }

    public static function acceptPartnerRequest($id)
    {
        list($id) = self::escape($id);

        $res = self::query("SELECT pn_partner,pn_partnerka FROM pary_navrh WHERE pn_id='$id'");

        if ($res) {
            $row = self::getSingleRow($res);
            self::newCouple($row['pn_partner'], $row['pn_partnerka']);
            self::query("DELETE FROM pary_navrh WHERE pn_id='$id'");
        }
    }

    public static function deletePartnerRequest($id)
    {
        self::query("DELETE FROM pary_navrh WHERE pn_id='?'", $id);
    }

    public static function editTridaBody(
        $p_id, $stt_trida, $stt_body, $stt_finale,
        $lat_trida, $lat_body, $lat_finale, $hodnoceni
    ) {
        self::query(
            "UPDATE pary
            SET p_stt_trida='?', p_stt_body='?', p_stt_finale='?',
                p_lat_trida='?', p_lat_body='?', p_lat_finale='?',
                p_hodnoceni='?'
            WHERE p_id='?'",
            $stt_trida, $stt_body, $stt_finale, $lat_trida, $lat_body,
            $lat_finale, $hodnoceni, $p_id
        );
    }
}
