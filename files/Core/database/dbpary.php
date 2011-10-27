<?php
class DBPary extends Database {
	public static function getPary() {
		$res = DBPary::query("SELECT * FROM pary ORDER BY p_aktu_vytvoreno ASC");
		return DBPary::getArray($res);
	}
	
	public static function getActivePary() {
		$res = DBPary::query(
		"SELECT p_id,
			m.u_id AS guy,m.u_id AS guy_id,m.u_jmeno AS guy_name,m.u_prijmeni AS guy_surname,
			f.u_id AS gal,f.u_id AS gal_id,f.u_jmeno AS gal_name,f.u_prijmeni AS gal_surname,
			p_stt_trida,p_stt_body,p_stt_finale,
			p_lat_trida,p_lat_body,p_lat_finale,p_hodnoceni
		FROM pary AS p
			LEFT JOIN users AS m ON p.p_id_partner=m.u_id
			LEFT JOIN users AS f ON p.p_id_partnerka=f.u_id
		WHERE p.p_archiv='0'
		ORDER BY p.p_aktu_vytvoreno ASC");
		return DBPary::getArray($res);
	}
	
	public static function getActiveParyByHodnoceni() {
		$res = DBPary::query(
		"SELECT p_id,
			m.u_id AS guy,m.u_id AS guy_id,m.u_jmeno AS guy_name,m.u_prijmeni AS guy_surname,
			f.u_id AS gal,f.u_id AS gal_id,f.u_jmeno AS gal_name,f.u_prijmeni AS gal_surname,
			p_stt_trida,p_stt_body,p_stt_finale,
			p_lat_trida,p_lat_body,p_lat_finale,p_hodnoceni
		FROM pary AS p
			LEFT JOIN users AS m ON p.p_id_partner=m.u_id
			LEFT JOIN users AS f ON p.p_id_partnerka=f.u_id
		WHERE p.p_archiv='0'
		ORDER BY p.p_hodnoceni DESC");
		return DBPary::getArray($res);
	}
	
	public static function getSinglePar($id) {
		list($id) = DBPary::escapeArray(array($id));
		
		$res = DBPary::query(
		"SELECT p_id,
			m.u_id AS guy_id,m.u_jmeno AS guy_name,m.u_prijmeni AS guy_surname,
			f.u_id AS gal_id,f.u_jmeno AS gal_name,f.u_prijmeni AS gal_surname,
			p_stt_trida,p_stt_body,p_stt_finale,
			p_lat_trida,p_lat_body,p_lat_finale,p_hodnoceni
		FROM pary AS p
			LEFT JOIN users AS m ON p.p_id_partner=m.u_id
			LEFT JOIN users AS f ON p.p_id_partnerka=f.u_id
		WHERE p.p_id='$id'");
		
		$array = DBPary::getArray($res);
		if($array)
			return $array[0];
		else
			return false;
	}
	
	public static function getLatestPartner($partner, $pohlavi) {
		list($partner, $pohlavi) = DBPary::escapeArray(array($partner, $pohlavi));
		
		if($pohlavi == "m") {
			$res = DBPary::query("SELECT p_id,p_id_partner,p_id_partnerka,u_id,u_jmeno,u_prijmeni,u_pohlavi " .
				"FROM pary LEFT JOIN users ON p_id_partnerka=u_id WHERE p_id_partner='$partner'" .
				" AND p_archiv='0'");
		} elseif($pohlavi == "f") {
			$res = DBPary::query("SELECT p_id,p_id_partner,p_id_partnerka,u_id,u_jmeno,u_prijmeni " .
				"FROM pary LEFT JOIN users ON p_id_partner=u_id WHERE p_id_partnerka='$partner'" .
				" AND p_archiv='0'");
		}
		$array = DBPary::getArray($res);
		if($array)
			return $array[0];
		else
			return false;
	}
	
	public static function getVysledky($p_id) {
		list($p_id) = DBPary::escapeArray(array($p_id));
		
		$res = DBPary::query("
			SELECT p_id,p_id_partner,p_id_partnerka,p_stt_trida,p_stt_body,p_stt_finale,
				p_lat_trida,p_lat_body,p_lat_finale,p_hodnoceni
			FROM pary
			WHERE p_id='$p_id' AND p_archiv='0'");
		
		$array = DBPary::getArray($res);
		if($array)
			return $array[0];
		else
			return false;
		
	}
	
	public static function newPartner($partner, $partnerka) {
		list($partner, $partnerka) = DBPary::escapeArray(array($partner, $partnerka));
		
		DBPary::query("
		UPDATE pary
		SET p_archiv='1',p_aktu_archivovano=NOW()
		WHERE (p_id_partner='$partner' OR p_id_partner='$partnerka' OR p_id_partnerka='$partnerka')
			AND p_archiv='0'");
		DBPary::query("INSERT INTO pary (p_id_partner, p_id_partnerka) VALUES ('$partner','$partnerka')");
	}
	
	public static function noPartner($partner) {
		list($partner) = DBPary::escapeArray(array($partner));
		
		DBPary::query(
		"DELETE n,r FROM nabidka_item AS n,rozpis_item AS r
		WHERE
			n.ni_partner=
				(SELECT p_id FROM pary
				WHERE (p_id_partner='$partner' OR p_id_partnerka='$partner') AND p_archiv='0')
			OR
			r.ri_partner=
				(SELECT p_id FROM pary
				WHERE (p_id_partner='$partner' OR p_id_partnerka='$partner') AND p_archiv='0')");
		DBPary::query("UPDATE pary SET p_archiv='1',p_aktu_archivovano=NOW()" .
			" WHERE (p_id_partner='$partner' OR p_id_partnerka='$partner') AND p_archiv='0'");
		
		DBPary::query("INSERT INTO pary (p_id_partner, p_id_partnerka) VALUES ('$partner','0')");
	}
	
	public static function getPartnerRequestsForMe($id) {
		list($id) = DBPary::escapeArray(array($id));
		
		$res = DBPary::query(
		"SELECT pn_id, pn_navrhl, u_jmeno, u_prijmeni, u_pohlavi
		FROM pary_navrh LEFT JOIN users ON pn_navrhl=u_id
		WHERE (pn_partner='$id' OR pn_partnerka='$id') AND pn_navrhl!='$id'");
		
		return DBPary::getArray($res);
	}
	
	public static function getPartnerRequestsByMe($id) {
		list($id) = DBPary::escapeArray(array($id));

		if(User::getUserPohlavi() == "m")
			$res = DBPary::query(
			"SELECT pn_id, u_id, u_jmeno, u_prijmeni, u_pohlavi
			FROM pary_navrh LEFT JOIN users ON pn_partnerka=u_id
			WHERE pn_partner='$id' AND pn_navrhl='$id'");
		else
			$res = DBPary::query(
			"SELECT pn_id, u_id, u_jmeno, u_prijmeni, u_pohlavi
			FROM pary_navrh LEFT JOIN users ON pn_partner=u_id
			WHERE pn_partnerka='$id' AND pn_navrhl='$id'");
		
		return DBPary::getArray($res);
	}
	
	public static function newPartnerRequest($navrhl, $partner, $partnerka) {
		list($navrhl, $partner, $partnerka) = DBPary::escapeArray(array($navrhl, $partner, $partnerka));
		
		DBPary::query("
		INSERT INTO pary_navrh
		(pn_navrhl, pn_partner, pn_partnerka)
		VALUES ($navrhl, $partner, $partnerka)");
	}
	
	public static function acceptPartnerRequest($id) {
		list($id) = DBPary::escapeArray(array($id));
		
		$res = DBPary::query("SELECT pn_partner,pn_partnerka FROM pary_navrh WHERE pn_id='$id'");
		
		if($res) {
			$row = DBPary::getSingleRow($res);
			DBPary::newPartner($row['pn_partner'], $row['pn_partnerka']);
			DBPary::query("DELETE FROM pary_navrh WHERE pn_id='$id'");
		}
	}
	
	public static function deletePartnerRequest($id) {
		list($id) = DBPary::escapeArray(array($id));
		
		DBPary::query("DELETE FROM pary_navrh WHERE pn_id='$id'");
	}
	
	public static function editTridaBody($p_id, $stt_trida, $stt_body, $stt_finale,
			$lat_trida, $lat_body, $lat_finale, $hodnoceni) {
		list($p_id, $stt_trida, $stt_body, $stt_finale, $lat_trida, $lat_body,
			$lat_finale, $hodnoceni) = DBPary::escapeArray(array($p_id, $stt_trida,
			$stt_body, $stt_finale, $lat_trida, $lat_body, $lat_finale, $hodnoceni));
		
		DBPary::query("UPDATE pary
		SET p_stt_trida='$stt_trida', p_stt_body='$stt_body', p_stt_finale='$stt_finale',
			p_lat_trida='$lat_trida', p_lat_body='$lat_body', p_lat_finale='$lat_finale',
			p_hodnoceni='$hodnoceni'
		WHERE p_id='$p_id'");
	}
}