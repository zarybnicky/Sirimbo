<?php
class DBPlatbyRaw extends Database {
	public static function insert($raw, $hash, $sorted, $discarded, $updateValues) {
		list($raw, $hash, $sorted, $discarded) =
			self::escape($raw, $hash, $sorted, $discarded);
		
		self::query(
			"INSERT INTO platby_raw
				(pr_raw,pr_hash,pr_sorted,pr_discarded)
			VALUES
				('$raw','$hash','$sorted','$discarded')
			ON DUPLICATE KEY UPDATE
				pr_id=LAST_INSERT_ID(pr_id)" .
			($updateValues ? ",pr_sorted=VALUES(pr_sorted),
				pr_discarded=VALUES(pr_discarded))" : '')
		);
	}
	public static function getUnsorted() {
		$res = self::query("SELECT * FROM platby_raw WHERE pr_sorted='0' AND pr_discarded='0'");
		return self::getArray($res);
	}
}