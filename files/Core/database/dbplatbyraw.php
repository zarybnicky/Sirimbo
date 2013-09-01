<?php
class DBPlatbyRaw extends Database {
	public static function insert($raw, $hash, $sorted, $discarded) {
		list($raw, $hash, $sorted, $discarded, $updateID) =
			self::escape($raw, $hash, $sorted, $discarded, $updateID);
		
		self::query(
			"INSERT INTO platby_raw
				(pr_raw,pr_hash,pr_sorted,pr_discarded)
			VALUES
				('$raw','$hash','$sorted','$discarded')
			ON DUPLICATE KEY UPDATE
				pr_id=LAST_INSERT_ID(pr_id),
				pr_sorted=VALUES(pr_sorted),
				pr_discarded=VALUES(pr_discarded)"
		);
	}
}