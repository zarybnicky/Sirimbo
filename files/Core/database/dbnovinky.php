<?php
class DBNovinky extends Database {
	public static function getNovinky() {
		$res = DBNovinky::query('SELECT * FROM novinky');
		
		return DBNovinky::getArray($res);
	}
	
	public static function getLastNovinky($number) {
		list($number) = DBNovinky::escapeArray(array($number));
		
		$res = DBNovinky::query("SELECT * FROM novinky ORDER BY no_id DESC LIMIT $number");
		
		return DBNovinky::getArray($res);
	}
	
	public static function addNovinka($text) {
		list($text) = DBNovinky::escapeArray(array($text));
		DBNovinky::query("INSERT INTO novinky (no_text) VALUES ('$text')");
	}
	
	public static function removeNovinka($id) {
		list($id) = DBNovinky::escapeArray(array($id));
		DBNovinky::query("DELETE FROM novinky WHERE no_id=$id");
	}
}