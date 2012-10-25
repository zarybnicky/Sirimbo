<?php
class DBSkupiny extends Database {
	public static function getSkupiny() {
		$res = DBSkupiny::query("SELECT * FROM users_skupiny");
		return DBSkupiny::getArray($res);
	}
	public static function getSingleSkupina($id) {
		list($id) = DBAkce::escapeArray(array($id));
		
		$res = DBSkupiny::query("SELECT * FROM users_skupiny WHERE us_id='$id'");
		return DBSkupiny::getSingleRow($res);
	}
	public static function addSkupina($color, $platba, $popis) {
		list($color, $platba, $popis) =
			DBAkce::escapeArray(array($color, $platba, $popis));
		
		DBSkupiny::query("INSERT INTO users_skupiny
		(us_color,us_platba_mesic,us_popis)
		VALUES ('$color','$platba','$popis')");
	}
	public static function editSkupina($id, $color, $platba, $popis) {
		list($id, $color, $platba, $popis) =
			DBAkce::escapeArray(array($id, $color, $platba, $popis));
		
		DBSkupiny::query("UPDATE users_skupiny
		SET us_color='$color',us_platba_mesic='$platba',us_popis='$popis' 
		WHERE us_id='$id'");
		return true;
	}
	public static function removeSkupina($id) {
		list($id) = DBAkce::escapeArray(array($id));
		
		DBSkupiny::query("DELETE FROM users_skupiny WHERE us_id='$id'");
		return true;
	}
}