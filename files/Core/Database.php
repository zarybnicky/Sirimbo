<?php
class Database {
	private static $connection = null;
	
	/* Sekce */
	/* USER */
	
	public static function checkUser($login, $pass) {
		list($login, $pass) = Database::escapeArray(array($login, $pass));
		
		$res = Database::query("SELECT COUNT(*) FROM users WHERE " .
			"u_login='$login' AND u_pass='$pass'");
		if(!$res) {
			return false;
		} else {
			$row = Database::getSingleRow($res);
			return $row["COUNT(*)"];
		}
	}
	
	public static function getUserlevel($login) {
		list($login) = Database::escapeArray(array($login));
		
		$res = Database::query("SELECT u_level FROM users WHERE " .
			"u_login='$login'");
		if(!$res) {
			return false;
		} else {
			$row = Database::getSingleRow($res);
			return $row["u_level"];
		}
	}
	
	public static function getUserID($login) {
		list($login) = Database::escapeArray(array($login));
		
		$res = Database::query("SELECT u_id FROM users WHERE " .
			"u_login='$login'");
		if(!$res) {
			return false;
		} else {
			$row = Database::getSingleRow($res);
			return $row["u_id"];
		}
	}
	
	public static function getUserData($login) {
		list($login) = Database::escapeArray(array($login));
		
		$res = Database::query("SELECT u_login,u_jmeno,u_pass,u_jmeno,u_prijmeni," .
			"u_email,u_telefon,u_poznamky,u_level,u_ban,u_lock FROM users WHERE u_login='$login'");
		if(!$res) {
			return false;
		} else {
			return Database::getSingleRow($res);
		}
	}
	
	public static function isUserBanned($login) {
		list($login) = Database::escapeArray(array($login));
		
		$res = Database::query("SELECT u_ban FROM users WHERE u_login='$login'");
		if(!$res) {
			return false;
		} else {
			$row = Database::getSingleRow($res);
			return $row["u_ban"];
		}
	}
	
	public static function isUserLocked($login) {
		list($login) = Database::escapeArray(array($login));
		
		$res = Database::query("SELECT u_lock FROM users WHERE u_login='$login'");
		if(!$res) {
			return false;
		} else {
			$row = Database::getSingleRow($res);
			return $row["u_lock"];
		}
	}
	
	public static function setUserData($login,$jmeno,$prijmeni,
		$email,$telefon,$poznamky,$level,$lock,$ban) {
		
		list($login, $jmeno, $prijmeni, $email, $telefon, $poznamky, $level, $lock, $ban) =
			Database::escapeArray(array($login, $jmeno, $prijmeni, $email, $telefon,
			$poznamky, $level, $lock, $ban));
		
			
		Database::query("UPDATE users SET " .
			"u_jmeno='$jmeno',u_prijmeni='$prijmeni',u_email='$email',u_telefon='$telefon'," .
			"u_poznamky='$poznamky',u_level='$level', u_lock='$lock', u_ban='$ban' WHERE u_login='$login'");
		return true;	
	}
	
	public static function addUser($login, $pass, $name, $surname, $email,
		$telefon, $poznamky, $level, $lock, $ban) {
		list($login, $pass, $name, $surname, $email, $telefon, $poznamky, $level, $lock, $ban) =
			Database::escapeArray(array($login, $pass, $name, $surname, $email, $telefon,
			$poznamky, $level, $lock, $ban));
		
		Database::query("INSERT INTO users " .
			"(u_login,u_pass,u_jmeno,u_prijmeni,u_email,u_telefon," .
			"u_poznamky,u_level,u_lock,u_ban) VALUES " .
			"('$login','$pass','$name','$surname','$email','$telefon'," .
			"'$poznamky','$level','$lock','$ban')");
		return true;
	}
	
	public static function removeUser($login) {
		list($login) = Database::escapeArray(array($login));
		
		$res = Database::query("SELECT u_id FROM users WHERE u_login='$login'");
		$row = Database::getSingleRow($res);
		$id = $row["u_id"];
		Database::query("DELETE FROM users WHERE u_id='$id'");
		Database::query("DELETE FROM rozpis WHERE r_trener='$id'");
		Database::query("DELETE FROM rozpis_item WHERE ri_partner='$id'");
		Database::query("DELETE FROM nabidka WHERE n_trener='$id'");
		Database::query("DELETE FROM nabidka_item WHERE ni_partner='$id'");
		Database::query("DELETE FROM upozorneni WHERE up_kdo='$id'");
		return true;
	}
	
	public static function getUsers() {
		$res = Database::query("SELECT * FROM users");
		return Database::getArray($res);
	}
	
	public static function getUserNames() {
		$res = Database::query("SELECT u_id,u_login,u_jmeno,u_prijmeni FROM users");
		return Database::getArray($res);
	}
	
	public static function getTrener() {
		$res = Database::query("SELECT u_id,u_jmeno,u_prijmeni FROM users WHERE u_level>=" . L_TRENER);
		return Database::getArray($res);
	}
	
	/* Sekce */
	/* NASTENKA */
	
	public static function getNastenka() {
		$res = Database::query("SELECT up_id,u_login,u_jmeno,u_prijmeni,up_aktu,up_nadpis,up_text,up_lock" .
			" FROM upozorneni LEFT JOIN users ON up_kdo=u_id");
		return Database::getArray($res);
	}
	
	public static function getNastenkaUserName($id) {
		list($id) = Database::escapeArray(array($id));
		
		$res = Database::query("SELECT u_login FROM upozorneni LEFT JOIN users ON up_kdo=u_id WHERE " .
			"up_id='$id'");
		if(!$res) {
			return false;
		} else {
			$row = Database::getSingleRow($res);
			return $row["u_login"];
		}
	}
	
	public static function getSingleNastenka($id) {
		list($id) = Database::escapeArray(array($id));
		
		$res = Database::query("SELECT u_login,u_jmeno,u_prijmeni,up_aktu,up_nadpis,up_text,up_lock" .
			" FROM upozorneni LEFT JOIN users ON up_kdo=u_id WHERE up_id='$id'");
		if(!$res) {
			return false;
		} else {
			return Database::getSingleRow($res);
		}
	}
	
	public static function isNastenkaLocked($id) {
		list($id) = Database::escapeArray(array($id));
		
		$res = Database::query("SELECT up_lock FROM upozorneni WHERE up_id='$id'");
		if(!$res) {
			return false;
		} else {
			$row = Database::getSingleRow($res);
			return $row["up_lock"];
		}
	}
	
	public static function addNastenka($user, $nadpis, $text, $lock) {
		list($user, $nadpis, $text, $lock) = Database::escapeArray(array($user, $nadpis, $text, $lock));
		
		Database::query("INSERT INTO upozorneni (up_kdo,up_nadpis,up_text,up_lock) VALUES " .
			"((SELECT u_id FROM users WHERE u_login='$user'),'$nadpis','$text','$lock')");
		
		return true;
	}
	
	public static function editNastenka($id, $nadpis, $text, $lock) {
		list($id, $nadpis, $text, $lock) = Database::escapeArray(array($id, $nadpis, $text, $lock));
		
		Database::query("UPDATE upozorneni SET " .
			"up_nadpis='$nadpis',up_text='$text',up_lock='$lock' WHERE up_id='$id'");
		
		return true;
	}

	public static function removeNastenka($id) {
		list($id) = Database::escapeArray(array($id));
		
		Database::query("DELETE FROM upozorneni WHERE up_id='$id'");
		
		return true;
	}
	
	/* Sekce */
	/* NABIDKA */
	
	public static function getNabidka() {
		$res = Database::query("SELECT u_jmeno,u_prijmeni,n_id,n_trener,n_pocet_hod,n_od,n_do,n_lock" .
			" FROM nabidka LEFT JOIN users ON n_trener=u_id");
		return Database::getArray($res);
	}
	
	public static function getNabidkaMaxLessons($id) {
		list($id) = Database::escapeArray(array($id));
		
		$res = Database::query("SELECT n_pocet_hod FROM nabidka WHERE n_id='$id'");
		if(!$res) {
			return false;
		} else {
			$row = Database::getSingleRow($res);
			return $row["n_pocet_hod"];
		}
	}
	
	public static function getSingleNabidka($id) {
		list($id) = Database::escapeArray(array($id));
		
		$res = Database::query("SELECT n_id,n_trener,u_jmeno,u_prijmeni,n_pocet_hod,n_od,n_do,n_lock" .
			" FROM nabidka LEFT JOIN users ON n_trener=u_id WHERE n_id='$id'");
		if(!$res) {
			return false;
		} else {
			return Database::getSingleRow($res);
		}
	}
	
	public static function getNabidkaTrenerID($id) {
		list($id) = Database::escapeArray(array($id));
		
		$res = Database::query("SELECT n_trener FROM nabidka WHERE n_id='$id'");
		if(!$res) {
			return false;
		} else {
			$row = Database::getSingleRow($res);
			return $row["n_trener"];
		}
	}
	
	public static function addNabidka($trener, $pocet_hod, $od, $do, $lock) {
		list($trener, $pocet_hod, $od, $do, $lock) =
			Database::escapeArray(array($trener, $pocet_hod, $od, $do, $lock));
		
		Database::query("INSERT INTO nabidka (n_trener,n_pocet_hod,n_od,n_do,n_lock) VALUES " .
			"('$trener','$pocet_hod','$od','$do','$lock')");
		
		return true;
	}
	
	public static function editNabidka($id, $trener, $pocet_hod, $od, $do, $lock) {
		list($id, $trener, $pocet_hod, $od, $do, $lock) =
			Database::escapeArray(array($id, $trener, $pocet_hod, $od, $do, $lock));
		
		Database::query("UPDATE nabidka SET n_trener='$trener',n_pocet_hod='$pocet_hod',n_od='$od'," .
			"n_do='$do',n_lock='$lock' WHERE n_id='$id'");
		
		return true;
	}
	
	public static function removeNabidka($id) {
		list($id) = Database::escapeArray(array($id));
		
		Database::query("DELETE FROM nabidka WHERE n_id='$id'");
		Database::query("DELETE FROM nabidka_item WHERE ni_id_rodic='$id'");
		
		return true;
		
	}
	
	public static function getNabidkaItem($parent_id) {
		list($parent_id) = Database::escapeArray(array($parent_id));
		
		$res = Database::query("SELECT u_id,u_login,u_jmeno,u_prijmeni,ni_id,ni_id_rodic,ni_partner," .
			"ni_pocet_hod,ni_lock FROM nabidka_item LEFT JOIN users ON ni_partner=u_id WHERE " .
			"ni_id_rodic='$parent_id'");
		return Database::getArray($res);
	}
	
	public static function getNabidkaItemLessons($parent_id) {
		list($parent_id) = Database::escapeArray(array($parent_id));
		
		$res = Database::query("SELECT SUM(ni_pocet_hod) FROM nabidka_item WHERE ni_id_rodic='$parent_id'");
		if(!$res) {
			return false;
		} else {
			$row = Database::getSingleRow($res);
			return $row["SUM(ni_pocet_hod)"];
		}
	}
	
	public static function hasNabidkaLessons($parent_id, $u_id) {
		list($parent_id, $u_id) = Database::escapeArray(array($parent_id, $u_id));
		
		$res = Database::query("SELECT ni_pocet_hod FROM nabidka_item WHERE ni_id_rodic='$parent_id' AND " .
			"ni_partner='$u_id'");
		if(!$res) {
			return false;
		} else {
			$row = Database::getSingleRow($res);
			return (bool)$row["ni_pocet_hod"];
		}
	}
	
	public static function addNabidkaItemLessons($login, $parent_id, $pocet_hod) {
		list($login, $parent_id, $pocet_hod) = Database::escapeArray(array($login, $parent_id, $pocet_hod));
		
		Database::query("INSERT INTO nabidka_item (ni_partner,ni_id_rodic,ni_pocet_hod)" .
			" VALUES ((SELECT u_id FROM users WHERE u_login='$login'),'$parent_id','$pocet_hod')" .
			" ON DUPLICATE KEY UPDATE ni_pocet_hod=ni_pocet_hod+'$pocet_hod'");
		
		return true;
	}
	
	public static function removeNabidkaItem($parent_id, $u_id) {
		list($parent_id, $u_id) = Database::escapeArray(array($parent_id, $u_id));
		
		Database::query("DELETE FROM nabidka_item WHERE ni_id_rodic='$parent_id' AND " .
			"ni_partner='$u_id'");
		
		return true;
	}
	
	/* Sekce */
	/* ROZPIS */
	
	public static function getRozpis() {
		$res = Database::query("SELECT u_jmeno,u_prijmeni,r_id,r_trener,r_kde,r_datum,r_od,r_do,r_lock" .
			" FROM rozpis LEFT JOIN users ON r_trener=u_id");
		return Database::getArray($res);
	}
	
	public static function rozpisSignUp($rid, $uid) {
		list($rid, $uid) = Database::escapeArray(array($rid, $uid));
		if(Database::isRozpisFree($rid)) {
			$res = Database::query("UPDATE rozpis_item SET ri_partner='$uid' WHERE ri_id='$rid'");
			return true;
		} else
			return false;
	}
	
	public static function rozpisSignOut($rid, $uid) {
		list($rid, $uid) = Database::escapeArray(array($rid, $uid));
		if(!Database::isRozpisFree($rid)) {
			$res = Database::query("UPDATE rozpis_item SET ri_partner='' WHERE ri_id='$rid'");
			return true;
		} else
			return false;
	}
	
	public static function getRozpisItem($rid) {
		list($rid) = Database::escapeArray(array($rid));
		
		$res = Database::query("SELECT u_id,u_login,u_jmeno,u_prijmeni,ri_id,ri_id_rodic,ri_partner," .
			"ri_od,ri_do,ri_lock FROM rozpis_item LEFT JOIN users ON ri_partner=u_id WHERE " .
			"ri_id_rodic='$rid'");
		return Database::getArray($res);
	}
	
	public static function getRozpisItemLesson($ri_id) {
		list($ri_id) = Database::escapeArray(array($ri_id));
		
		$res = Database::query("SELECT u_id,u_login,u_jmeno,u_prijmeni,r_trener,ri_id,ri_id_rodic,ri_partner," .
			"ri_od,ri_do,ri_lock FROM rozpis_item LEFT JOIN users ON ri_partner=u_id LEFT JOIN rozpis ON ri_id_rodic=r_id WHERE " .
			"ri_id='$ri_id'");
		return Database::getArray($res);
	}

	public static function isRozpisFree($rid) {
	list($rid) = Database::escapeArray(array($rid));
		
		$res = Database::query("SELECT ri_partner FROM rozpis_item WHERE ri_id='$rid'");
		if(!$res) {
			return false;
		} else {
			$row = Database::getSingleRow($res);
			return !(bool)$row["ri_partner"];
		}
	}
	
	public static function getSingleRozpis($id) {
		list($id) = Database::escapeArray(array($id));
		
		$res = Database::query("SELECT r_id,r_trener,u_jmeno,u_prijmeni,r_kde,r_datum,r_od,r_do,r_lock" .
			" FROM rozpis LEFT JOIN users ON r_trener=u_id WHERE r_id='$id'");
		if(!$res) {
			return false;
		} else {
			return Database::getSingleRow($res);
		}
	}
	
	public static function getRozpisTrenerID($id) {
		list($id) = Database::escapeArray(array($id));
		
		$res = Database::query("SELECT r_trener FROM rozpis WHERE r_id='$id'");
		if(!$res) {
			return false;
		} else {
			$row = Database::getSingleRow($res);
			return $row["r_trener"];
		}
	}
	
	public static function addRozpis($trener, $kde, $datum, $od, $do, $lock) {
		list($trener, $kde, $datum, $od, $do, $lock) =
			Database::escapeArray(array($trener, $kde, $datum, $od, $do, $lock));
		
		Database::query("INSERT INTO rozpis (r_trener,r_kde,r_datum,r_od,r_do,r_lock) VALUES " .
			"('$trener','$kde','$datum','$od','$do','$lock')");
		
		return true;
	}
	
	public static function editRozpis($id, $trener, $kde, $datum, $od, $do, $lock) {
		list($id, $trener, $kde, $datum, $od, $do, $lock) =
			Database::escapeArray(array($id, $trener, $kde, $datum, $od, $do, $lock));
		
		Database::query("UPDATE rozpis SET r_trener='$trener',r_kde='$kde',r_datum='$datum'," .
			"r_od='$od',r_do='$do',r_lock='$lock' WHERE r_id='$id'");
		
		return true;
	}
	
	public static function removeRozpis($id) {
		list($id) = Database::escapeArray(array($id));
		
		Database::query("DELETE FROM rozpis WHERE r_id='$id'");
		Database::query("DELETE FROM rozpis_item WHERE ri_id_rodic='$id'");
		
		return true;
	}
	
	/* Sekce */
	/* DOKUMENTY */
	
	public static function getDokumenty() {
		$res = Database::query("SELECT u_jmeno,u_prijmeni,d_id,d_path,d_name,d_filename,d_kategorie,d_kdo" .
			" FROM dokumenty LEFT JOIN users ON d_kdo=u_id");
		return Database::getArray($res);
	}
	
	public static function getSingleDocument($id) {
		list($id) = Database::escapeArray(array($id));
		//TODO: Handle database returning FALSE
		$res = Database::query("SELECT u_jmeno,u_prijmeni,d_id,d_path,d_name,d_filename,d_kategorie,d_kdo" .
			" FROM dokumenty LEFT JOIN users ON d_kdo=u_id WHERE d_id='$id'");
		if(!$res) {
			return false;
		} else {
			return Database::getSingleRow($res);
		}
	}
	
	public static function getDocumentPath($id) {
		list($id) = Database::escapeArray(array($id));
		
		$res = Database::query("SELECT d_path FROM dokumenty WHERE d_id='$id'");
		if(!$res) {
			return false;
		} else {
			$row = Database::getSingleRow($res);
			return $row["d_path"];
		}
	}
	
	public static function getDokumentUserID($id) {
		list($id) = Database::escapeArray(array($id));
		
		$res = Database::query("SELECT d_kdo FROM dokumenty WHERE d_id='$id'");
		if(!$res) {
			return false;
		} else {
			$row = Database::getSingleRow($res);
			return $row["d_kdo"];
		}
	}
	
	public static function addDokument($path, $name, $filename, $kategorie, $kdo) {
		list($path, $kategorie, $kdo) = Database::escapeArray(array($path, $kategorie, $kdo));
		
		Database::query("INSERT INTO dokumenty (d_path,d_name,d_filename,d_kategorie,d_kdo) VALUES " .
			"('$path','$name','$filename','$kategorie','$kdo')");
		return true;
	}
	
	public static function editDokument($id, $newname) {
		list($id, $newname) = Database::escapeArray(array($id, $newname));
		
		Database::query("UPDATE dokumenty SET d_name='$newname' WHERE d_id='$id'");
		
		return true;
	}
	
	public static function removeDokument($id) {
		list($id) = Database::escapeArray(array($id));
		
		Database::query("DELETE FROM dokumenty WHERE d_id='$id'");
		
		return true;
	}
	
	/* Sekce */
	/* INTERNAL */
	
	private static function escapeArray($array) {
		Database::getConnection();
		$escape = implode("%%%%%", $array);
		$escape = mysql_real_escape_string($escape);
		return explode("%%%%%", $escape);
	}
	
	private static function getConnection() {
		if(Database::$connection != null)
			return;
		Database::$connection = @mysql_connect(DB_SERVER, DB_USER, DB_PASS)
			or View::viewError(ER_DATABASE_CONNECTION);
		@mysql_select_db(DB_DATABASE, Database::$connection)
			or View::viewError(ER_DATABASE_CONNECTION);
		@mysql_set_charset("utf8", Database::$connection)
			or View::viewError(ER_DATABASE_CONNECTION);
	}
	
	private static function query($query) {
		Database::getConnection();
		$res = mysql_query($query, Database::$connection);
		if(!$res)
			View::viewError(ER_DATABASE);
		return $res;
	}
	
	private static function getSingleRow($resource) {
		return mysql_fetch_assoc($resource);
	}
	
	private static function getArray($resource) {
		$result = array();
		$rows = mysql_num_rows($resource);
		
		for($i = 0; $i < $rows; $i++) {
			$result[] = @mysql_fetch_assoc($resource);
		}
		
		return $result;
	}
}
?>