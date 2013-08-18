<?php
class DBSkupiny extends Database {
	public static function getSkupiny($orderByCount = false) {
		if(!$orderByCount)
			$res = DBSkupiny::query("SELECT * FROM users_skupiny");
		else
			$res = DBSkupiny::query(
				"SELECT users_skupiny.*,COUNT(*) AS us_count
				FROM users_skupiny
					LEFT JOIN users on us_id=u_skupina
				GROUP BY us_id
				ORDER BY us_count DESC"
			);
		return DBSkupiny::getArray($res);
	}
	public static function getSingleSkupina($id) {
		list($id) = DBSkupiny::escapeArray(array($id));
		
		$res = DBSkupiny::query("SELECT * FROM users_skupiny WHERE us_id='$id'");
		return DBSkupiny::getSingleRow($res);
	}
	public static function addSkupina($color, $platba, $platba_ctvrtrok,
			$platba_pulrok, $popis) {
		list($color, $platba, $platba_ctvrtrok, $platba_pulrok, $popis) =
			DBSkupiny::escapeArray(array($color, $platba, $platba_ctvrtrok,
			$platba_pulrok, $popis));
		
		DBSkupiny::query("INSERT INTO users_skupiny
		(us_color,us_platba_mesic,us_platba_ctvrtrok,us_platba_pulrok,us_popis)
		VALUES ('$color','$platba','$platba_ctvrtrok','$platba_pulrok','$popis')");
	}
	public static function editSkupina($id, $color, $platba, $platba_ctvrtrok,
			$platba_pulrok, $popis) {
		list($id, $color, $platba, $platba_ctvrtrok, $platba_pulrok, $popis) =
			DBSkupiny::escapeArray(array($id, $color, $platba, $platba_ctvrtrok,
			$platba_pulrok, $popis));
		
		DBSkupiny::query("UPDATE users_skupiny
		SET us_color='$color',us_platba_mesic='$platba',
			us_platba_ctvrtrok='$platba_ctvrtrok',
			us_platba_pulrok='$platba_pulrok',us_popis='$popis' 
		WHERE us_id='$id'");
		return true;
	}
	public static function removeSkupina($id) {
		list($id) = DBSkupiny::escapeArray(array($id));
		
		DBSkupiny::query("DELETE FROM users_skupiny WHERE us_id='$id'");
		return true;
	}
}