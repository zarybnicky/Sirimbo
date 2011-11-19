<?php
class DBAnkety extends Database {
	public static function getAnkety() {
		$res = DBAnkety::query('SELECT * FROM ankety');
		return DBAnkety::getArray($res);
	}
	
	public static function getSingleAnketa($id) {
		list($id) = DBAnkety::escapeArray(array($id));
		
		$res = DBAnkety::query("SELECT * FROM ankety WHERE ak_id='$id'");
		return DBAnkety::getSingleRow($res);
	}
	
	public static function getLatestAnketa() {
		$res = DBAnkety::query("SELECT * FROM ankety ORDER BY ak_id DESC LIMIT 1");
		return DBAnkety::getSingleRow($res);
	}
	
	public static function getVisibleAnketyWithItems() {
		return DBAnkety::getAnketyWithItems(true);
	}
	
	public static function getAvailableAnketyWithItems($ip) {
		return DBAnkety::getAnketyWithItems(true, $ip);
	}
	
	public static function getAnketyWithItems($visible = false, $ip = 0) {
		list($visible, $ip) = DBAnkety::escapeArray(array($visible, $ip));
		
		$res = DBAnkety::query(
			"SELECT ak_id,ak_kdo,ak_jmeno,ak_text,ak_pristup,aki_id,aki_text,aki_pocet
			FROM ankety AS ak
				LEFT JOIN ankety_item AS aki ON ak_id=aki_id_rodic
			WHERE 1=1" . ($visible ? " AND ak_visible='1'" : "") . ($ip > 0 ?
			" AND NOT EXISTS (SELECT akp_id FROM ankety_ip WHERE akp_id_rodic=ak.ak_id AND akp_ip=INET_ATON('$ip'))" : "")
		);
		$array = DBAnkety::getArray($res);
		$result = array();
		
		$latest = -1;
		foreach($array as $row) {
			if($latest == $row['ak_id'])  {
				$result[count($result)-1]['items'][] = array(
					'aki_id' => $row['aki_id'],
					'aki_text' => $row['aki_text'],
					'aki_pocet' => $row['aki_pocet']
				);
			} else {
				$result[] = array(
					'ak_id' => $row['ak_id'],
					'ak_kdo' => $row['ak_kdo'],
					'ak_jmeno' => $row['ak_jmeno'],
					'ak_text' => $row['ak_text'],
					'ak_pristup' => $row['ak_pristup']
				);
				$result[count($result)-1]['items'][] = array(
					'aki_id' => $row['aki_id'],
					'aki_text' => $row['aki_text'],
					'aki_pocet' => $row['aki_pocet']
				);
				$latest = $row['ak_id'];
			}
		}
		return $result;
	}
	
	public static function addAnketa($kdo, $jmeno, $text, $pristup, $visible) {
		list($kdo, $jmeno, $text, $pristup, $visible) =
			DBAnkety::escapeArray(array($kdo, $jmeno, $text, $pristup, $visible));
		
		$res = DBAnkety::query('INSERT INTO ankety (ak_kdo,ak_jmeno,ak_text,ak_pristup,ak_visible) VALUES ' .
			"('$kdo','$jmeno','$text','$pristup','$visible')");
		return true;
	}
	
	public static function editAnketa($id, $jmeno, $text, $pristup, $visible) {
		list($id, $jmeno, $text, $pristup, $visible) =
			DBAnkety::escapeArray(array($id, $jmeno, $text, $pristup, $visible));
		
		$res = DBAnkety::query("UPDATE ankety SET ak_jmeno='$jmeno',ak_text='$text',ak_pristup='$pristup'," .
			"ak_visible='$visible' WHERE ak_id='$id'");
		//TODO: novinky - s novým názvem (NE $data)
		return true;
	}
	
	public static function removeAnketa($id) {
		list($id) = DBAnkety::escapeArray(array($id));
		
		$res = DBAnkety::query("DELETE FROM ankety WHERE ak_id='$id'");
		$res = DBAnkety::query("DELETE FROM ankety_item WHERE aki_id_rodic='$id'");
		$res = DBAnkety::query("DELETE FROM ankety_ip WHERE akp_id_rodic='$id'");
		return true;
	}
	
	public static function getAnketaItems($id) {
		list($id) = DBAnkety::escapeArray(array($id));
		
		$res = DBAnkety::query("SELECT * FROM ankety_item WHERE aki_id_rodic='$id'");
		return DBAnkety::getArray($res);
	}
	
	public static function addAnketaItem($id_rodic, $text) {
		list($id_rodic, $text) = DBAnkety::escapeArray(array($id_rodic, $text));
		
		$res = DBAnkety::query("INSERT INTO ankety_item (aki_id_rodic,aki_text,aki_pocet) VALUES " .
			"('$id_rodic','$text','0')");
		return true;
	}
	
	public static function editAnketaItem($id, $text) {
		list($id, $text) =
			DBAnkety::escapeArray(array($id, $text));
		
		$res = DBAnkety::query("UPDATE ankety_item SET aki_text='$text' WHERE aki_id='$id'");
		return true;
	}
	
	public static function removeAnketaItem($id) {
		list($id) = DBAnkety::escapeArray(array($id));
		
		$res = DBAnkety::query("DELETE FROM ankety_item WHERE aki_id='$id'");
		return true;
	}
	
	public static function isUniqueIP($id, $ip) {
		list($id, $ip) = DBAnkety::escapeArray(array($id, $ip));
		
		$res = DBAnkety::query("SELECT akp_id FROM ankety_ip" .
			" WHERE akp_id_rodic='$id' AND akp_ip=INET_ATON('$ip')");
		$row = DBAnkety::getSingleRow($res);
		
		return !($row && $row['akp_id']);
	}
	
	public static function vote($id, $choice, $ip) {
		list($id, $choice, $ip) = DBAnkety::escapeArray(array($id, $choice, $ip));
		
		$res = DBAnkety::query("UPDATE ankety_item SET aki_pocet=aki_pocet+1 WHERE aki_id='$choice'");
		$res = DBAnkety::query("INSERT INTO ankety_ip (akp_id_rodic,akp_ip) VALUES" .
			" ('$id', INET_ATON('$ip')) ON DUPLICATE KEY UPDATE akp_aktu=CURRENT_TIMESTAMP");
		return true;
	}
}