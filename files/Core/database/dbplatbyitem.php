<?php
class DBPlatbyItem extends Database {
	public static function insert($uid, $cid, $rid, $amount, $date) {
		list($uid, $cid, $rid, $amount, $date) = self::escape($uid, $cid, $rid, $amount, $date);
		
		self::query(
			"INSERT INTO platby_item
				(pi_id_user,pi_id_category,pi_id_raw,pi_amount,pi_date)
			VALUES
				('$uid','$cid','$rid','$amount','$date')
			ON DUPLICATE KEY UPDATE
				pi_id_user=VALUES(pi_id_user),
				pi_id_category=VALUES(pi_id_category),
				pi_amount=VALUES(pi_amount),
				pi_date=VALUES(pi_date)"
		);
	}
	public static function getSingle($id) {
		list($id) = self::escape($id);
		
		$res = self::query("SELECT * FROM platby_item WHERE pi_id='$id'");
		return self::getSingleRow($res);
	}
	public static function getSingleByRawId($id) {
		list($id) = self::escape($id);
		
		$res = self::query("SELECT * FROM platby_item WHERE pi_id_raw='$id'");
		return self::getSingleRow($res);
	}
}