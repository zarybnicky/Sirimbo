<?php
class DBPary extends Database {
	public static function getPary() {
		$res = DBPary::query("SELECT * FROM pary ORDER BY p.p_aktu_vytvoreno ASC");
		return DBPary::getArray($res);
	}
	
	public static function getActivePary() {
		$res = DBPary::query(
		"SELECT p_id,
			m.u_id AS guy,m.u_id AS guy_id,m.u_jmeno AS guy_name,m.u_prijmeni AS guy_surname,
			f.u_id AS gal,f.u_id AS gal_id,f.u_jmeno AS gal_name,f.u_prijmeni AS gal_surname
		FROM pary AS p
			LEFT JOIN users AS m ON p.p_id_partner=m.u_id
			LEFT JOIN users AS f ON p.p_id_partnerka=f.u_id
		WHERE p.p_archiv='0'
		ORDER BY p.p_aktu_vytvoreno ASC");
		return DBPary::getArray($res);
	}
	
	public static function getSinglePar($id) {
		list($id) = DBPary::escapeArray(array($id));
		
		$res = DBPary::query(
		"SELECT p_id,
			m.u_id AS guy_id,m.u_jmeno AS guy_name,m.u_prijmeni AS guy_surname,
			f.u_id AS gal_id,f.u_jmeno AS gal_name,f.u_prijmeni AS gal_surname
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
		
		DBPary::query("UPDATE pary SET p_archiv='1',p_aktu_archivovano=NOW()" .
			" WHERE (p_id_partner='$partner' OR p_id_partnerka='$partner') AND p_archiv='0'");
		
		DBPary::query("INSERT INTO pary (p_id_partner, p_id_partnerka) VALUES ('$partner','0')");
	}
	
	public static function getLatestPartner($partner, $pohlavi) {
		if($pohlavi == "m") {
			$res = DBPary::query("SELECT p_id,p_id_partner,p_id_partnerka,u_id,u_jmeno,u_prijmeni " .
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
}