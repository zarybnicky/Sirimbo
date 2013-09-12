<?php
class DBSkupiny extends Database {
	public static function unlinkGroup($sid, $gid) {
		list($sid, $gid) = self::escape($sid, $gid);
		self::query(
				"DELETE FROM platby_group_skupina
				WHERE pgs_id_group='$gid' AND pgs_id_skupina='$sid'"
		);
	}
	public static function getNotInGroup($id) {
		list($id) = self::escape($id);
		$res = self::query(
				"SELECT * FROM skupiny
				WHERE NOT EXISTS (
					SELECT pgs_id FROM platby_group_skupina WHERE pgs_id_skupina=s_id AND pgs_id_group='$id'
				)");
		return self::getArray($res);
	}
	public static function get() {
		$res = self::query("SELECT * FROM skupiny");
		return self::getArray($res);
	}
	public static function getSingle($id) {
		list($id) = self::escape($id);
		$res = self::query("SELECT * FROM skupiny WHERE s_id='$id'");
		return self::getSingleRow($res);
	}
	public static function insert($name, $color, $desc) {
		list($name, $color, $desc) = self::escape($name, $color, $desc);
		self::query(
				"INSERT INTO skupiny
					(s_name,s_color_text,s_color_rgb,s_description)
				VALUES
					('$name','$color','FFFFFF','$desc')"
		);
	}
	public static function update($id, $name, $color, $desc) {
		list($id, $name, $color, $desc) = self::escape($id, $name, $color, $desc);
		self::query(
				"UPDATE skupiny SET
					s_name='$name',s_color_text='$color',s_description='$desc'
				WHERE s_id='$id'"
		);
	}
	public static function delete($id) {
		list($id) = self::escape($id);
		self::query("DELETE FROM skupiny WHERE s_id='$id'");
		return true;
	}
}