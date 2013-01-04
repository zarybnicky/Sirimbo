<?php
class DBAktuality extends Database {
	public static function getAktuality($kat = 0, $kdo = 0) {
		list($kat) = DBAktuality::escapeArray(array($kat));
		
		$res = DBAktuality::query(
			"SELECT *
			FROM aktuality
			WHERE 1=1" . ($kat != 0 ? " AND at_kat='$kat'" : '') .
			($kdo > 0 ? " AND at_kdo='$kdo'" : '') .
			" ORDER BY at_aktu DESC"
		);
		return DBAktuality::getArray($res);
	}
	public static function getSingleAktualita($id) {
		list($id) = DBAktuality::escapeArray(array($id));
		
		$res = DBAktuality::query(
			"SELECT *
			FROM aktuality
			WHERE at_id='$id'"
		);
		return DBAktuality::getSingleRow($res);
	}
	public static function addAktualita($kdo, $kat, $jmeno, $text, $preview, $foto, $foto_main) {
		list($kdo, $kat, $jmeno, $text, $preview, $foto, $foto_main) =
			DBAktuality::escapeArray(array($kdo, $kat, $jmeno, $text, $preview, $foto, $foto_main));
		
		DBAktuality::query(
			"INSERT INTO aktuality (at_kdo,at_kat,at_jmeno,at_text,at_preview,at_foto,at_foto_main)
			VALUES ('$kdo','$kat','$jmeno','$text','$preview','$foto','$foto_main')"
		);
	}
	public static function editAktualita($id, $kat, $jmeno, $text, $preview, $foto, $foto_main) {
		list($id, $kat, $jmeno, $text, $preview, $foto, $foto_main) =
			DBAktuality::escapeArray(array($id, $kat, $jmeno, $text, $preview, $foto, $foto_main));
		
		DBAktuality::query(
			"UPDATE aktuality SET at_kat='$kat',at_jmeno='$jmeno',at_text='$text',
			at_preview='$preview',at_foto='$foto',at_foto_main='$foto_main'
			WHERE at_id='$id'"
		);
	}
	public static function removeAktualita($id) {
		list($id) = DBAktuality::escapeArray(array($id));
		
		DBAktuality::query(
			"DELETE FROM aktuality WHERE at_id='$id'"
		);
	}
	public static function getAktualityFoto($id) {
		list($id) = DBAktuality::escapeArray(array($id));
		
		$res = DBAktuality::query(
			"SELECT * FROM aktuality_foto WHERE af_id_rodic='$id'"
		);
		return DBAktuality::getArray($res);
	}
}