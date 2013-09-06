<?php
class DBPlatbyCategory extends Database {
	public static function get() {
		$res = self::query('SELECT * FROM platby_category');
		return self::getArray($res);
	}
}