<?php
class DBPlatbyCategory extends Database {
	public static function getCategories() {
		$res = self::query('SELECT * FROM platby_category');
		return self::getArray($res);
	}
	public static function getCategoryLookup() {
		$in = self::getCategories();
		$out = array();
		foreach($in as $array) {
			$out[(int) $array['pc_symbol']] = $array;
		}
		return $out;
	}
}