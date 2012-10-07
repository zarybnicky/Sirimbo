<?php
class DBInzerce extends Database {
	public static function getInzerce($visible = true, $confirmed = true, $kategorie = INZERCE_ALL,
			$pocet = 0, $vlastnik = 0) {
		list($visible, $confirmed, $kategorie, $pocet, $vlastnik) =
			DBInzerce::escapeArray(array($visible, $confirmed, $kategorie, $pocet, $vlastnik));
		
		$res = DBInzerce::query(
			"SELECT *
			FROM inzerce
			WHERE 1=1" . ($visible ? " AND i_visible='1' AND i_do>=" . date('Y-m-d') : "") .
				($confirmed > 0 ? " AND i_confirmed='1'" : " AND i_confirmed='0'") .
				($kategorie != INZERCE_ALL ? " AND i_kat='$kategorie'" : "") .
				($vlastnik > 0 ? " AND i_reg='$vlastnik'" : "") .
			" ORDER BY i_od DESC" . ($pocet > 0 ? " LIMIT $pocet" : "")
		);
		return DBInzerce::getArray($res);
	}
	
	public static function getSingleInzerat($id) {
		list($id) = DBInzerce::escapeArray(array($id));
		
		$res = DBInzerce::query(
			"SELECT *
			FROM inzerce
			WHERE i_id='$id'"
		);
		return DBInzerce::getSingleRow($res);
		
	}
	
	public static function addInzerat($kat, $userid, $jmeno, $prijmeni, $nadpis, $text, $foto,
			$od, $do, $pass, $visible, $confirmed) {
		list($kat, $userid, $jmeno, $prijmeni, $nadpis, $text, $foto, $od, $do, $pass, $visible, $confirmed) =
			DBInzerce::escapeArray(array($kat, $userid, $jmeno, $prijmeni, $nadpis, $text, $foto,
			$od, $do, $pass, $visible, $confirmed));
		
		$res = DBInzerce::query(
			"INSERT INTO inzerce (i_kat,i_reg,i_jmeno,i_prijmeni,i_nadpis,i_text,i_foto,i_od,i_do,i_pass,
				i_visible,i_confirmed)
			VALUES ('$kat','$userid','$jmeno','$prijmeni','$nadpis','$text','$foto','$od','$do','$pass',
				'$visible','$confirmed')"
		);
		return true;
	}
	
	public static function editInzerat($id, $kat, $userid, $jmeno, $prijmeni, $nadpis, $text, $foto,
			$od, $do, $pass, $visible, $confirmed) {
		list($id, $kat, $userid, $jmeno, $prijmeni, $nadpis, $text, $foto, $od, $do, $pass, $visible, $confirmed) =
			DBInzerce::escapeArray(array($id, $kat, $userid, $jmeno, $prijmeni, $nadpis, $text, $foto,
			$od, $do, $pass, $visible, $confirmed));
		
		$res = DBInzerce::query(
			"UPDATE inzerce SET i_kat='$kat',i_reg='$userid',i_jmeno='$jmeno',i_prijmeni='$prijmeni',
				i_nadpis='$nadpis',i_text='$text',i_foto='$foto',i_od='$od',i_do='$do',i_pass='$pass',
				i_visible='$visible',i_confirmed='$confirmed'
			WHERE i_id='$id'"
		);
		return true;
	}
	
	public static function removeInzerat($id) {
		list($id) = DBInzerce::escapeArray(array($id));
		
		DBInzerce::query("DELETE FROM inzerce WHERE i_id='$id'");
		return true;
	}
	
	public static function confirmInzerat($id, $visible) {
		list($id, $visible) = DBInzerce::escapeArray(array($id, $visible));
		
		DBInzerce::query("UPDATE inzerce SET i_visible='$visible',i_confirmed='1' WHERE i_id='$id'");
		return true;
	}
}
?>