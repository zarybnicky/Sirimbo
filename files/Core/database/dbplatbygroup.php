<?php
class DBPlatbyGroup extends Database {
	public static function addChild($gid, $cid) {
		list($gid, $cid) = self::escape($gid, $cid);
		self::query(
				"INSERT IGNORE INTO platby_category_group
				(pcg_id_group,pcg_id_category)
				VALUES
				('$gid','$cid')"
		);
	}
	public static function removeChild($gid, $cid) {
		list($gid, $cid) = self::escape($gid, $cid);
		self::query(
				"DELETE FROM platby_category_group
				WHERE pcg_id_group='$gid' AND pcg_id_category='$cid'"
		);
	}
	public static function insert($type, $name, $desc, $base) {
		list($type, $name, $desc, $base) = self::escape($type, $name, $desc, $base);
		
		self::query(
				"INSERT INTO platby_group
				(pg_type,pg_name,pg_description, pg_base)
				VALUES
				('$type','$name','$desc','$base')"
		);
	}
	public static function update($id, $type, $name, $desc, $base) {
		list($id, $type, $name, $desc, $base) = self::escape($id, $type, $name, $desc, $base);
		
		self::query(
				"UPDATE platby_group
				SET pg_type='$type',pg_name='$name',pg_description='$desc',pg_base='$base'
				WHERE pg_id='$id'"
		);
	}
	public static function delete($id) {
		list($id) = self::escape($id);
		self::query(
				"DELETE FROM platby_group
				WHERE pg_id='$id'"
		);
	}
	public static function getNotInCategory($id) {
		list($id) = self::escape($id);
		$res = self::query(
				"SELECT * FROM platby_group
				WHERE NOT EXISTS (
					SELECT pcg_id FROM platby_category_group WHERE pcg_id_group=pg_id AND pcg_id_category='$id'
				)");
		return self::getArray($res);
	}
	public static function getNotInSkupina($id) {
		list($id) = self::escape($id);
		$res = self::query(
				"SELECT * FROM platby_group
				WHERE NOT EXISTS (
					SELECT pgs_id FROM platby_group_skupina WHERE pgs_id_group=pg_id AND pgs_id_skupina='$id'
				)");
		return self::getArray($res);
	}
	public static function getGroups() {
		$res = self::query(
				'SELECT *
				FROM platby_group
				ORDER BY pg_type,pg_id'
		);
		return self::getArray($res);
	}
	public static function getGroupsWithCategories() {
		$res = self::query(
				"SELECT *
				FROM platby_category_group
					LEFT OUTER JOIN platby_group ON pcg_id_group=pg_id
					LEFT OUTER JOIN platby_category ON pcg_id_category=pc_id
				WHERE pc_archive='0'
				ORDER BY pg_type,pg_id,pc_symbol"
		);
		return self::getArray($res);
	}
	public static function getSingle($id) {
		list($id) = self::escape($id);
		$res = self::query(
				"SELECT *
				FROM platby_group
				WHERE pg_id='$id'"
		);
		return self::getSingleRow($res);
	}
	public static function getSingleWithCategories($id) {
		list($id) = self::escape($id);
		$res = self::query(
				"SELECT *
				FROM platby_category_group
					LEFT JOIN platby_group ON pcg_id_group=pg_id
					LEFT JOIN platby_category ON pcg_id_category=pc_id
				WHERE pg_id='$id'
				ORDER BY pg_type,pg_id,pc_symbol"
		);
		return self::getArray($res);
	}
	public static function getSingleWithSkupiny($id) {
		list($id) = self::escape($id);
		$res = self::query(
				"SELECT *
				FROM platby_group_skupina
					LEFT JOIN platby_group ON pgs_id_group=pg_id
					LEFT JOIN skupiny ON pgs_id_skupina=s_id
				WHERE pg_id='$id'
				ORDER BY pg_type,pg_id"
		);
		return self::getArray($res);
	}
	public static function getWithoutSkupina() {
		$res = self::query(
				'SELECT * FROM platby_group
				WHERE NOT EXISTS (
					SELECT pgs_id FROM platby_group_skupina WHERE pgs_id_group=pg_id
				)');
		return self::getArray($res);
	}
	public static function getWithoutCategory() {
		$res = self::query(
				'SELECT * FROM platby_group
				WHERE NOT EXISTS (
					SELECT pcg_id FROM platby_category_group WHERE pcg_id_group=pg_id
				)');
		return self::getArray($res);
	}
}