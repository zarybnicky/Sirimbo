<?php
class DBPlatbyGroup extends Database {
	public static function getGroupsWithCategories() {
		$res = self::query(
				'SELECT *
				FROM platby_category_group
					LEFT JOIN platby_group ON pcg_id_group=pg_id
					LEFT JOIN platby_category ON pcg_id_category=pc_id
				ORDER BY pg_type,pg_id,pc_symbol'
		);
		return self::getArray($res);
	}
}