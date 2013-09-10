<?php
class DBPlatbyCategory extends Database {
	public static function get() {
		$res = self::query('SELECT * FROM platby_category');
		return self::getArray($res);
	}
	public static function getOrphan() {
		$res = self::query(
				'SELECT * FROM platby_category
				WHERE NOT EXISTS (
					SELECT pcg_id FROM platby_category_group WHERE pcg_id_category=pc_id
				)');
		return self::getArray($res);
	}
	public static function getNotInGroup($id) {
		list($id) = self::escape($id);
		$res = self::query(
				"SELECT * FROM platby_category
				WHERE NOT EXISTS (
					SELECT pcg_id FROM platby_category_group WHERE pcg_id_category=pc_id AND pcg_id_group='$id'
				)");
		return self::getArray($res);
	}
}