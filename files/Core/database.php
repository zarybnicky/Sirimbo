<?php
class Database {
	private static $connection = null;

	public static function getInstance() { return new self(); }
	
	protected static function escapeArray($array) {
		Database::getConnection();
		$escape = implode("%%%%%", $array);
		$escape = mysql_real_escape_string($escape);
		return explode("%%%%%", $escape);
	}
	
	protected static function getConnection() {
		if(Database::$connection != null)
			return;
		Database::$connection = @mysql_connect(DB_SERVER, DB_USER, DB_PASS)
			or View::viewError(ER_DATABASE_CONNECTION);
		@mysql_select_db(DB_DATABASE, Database::$connection)
			or View::viewError(ER_DATABASE_CONNECTION);
		@mysql_set_charset("utf8", Database::$connection)
			or View::viewError(ER_DATABASE_CONNECTION);
	}
	
	protected static function query($query) {
		Database::getConnection();
		$res = mysql_query($query, Database::$connection);
		if(!$res)
			View::viewError(ER_DATABASE);
		return $res;
	}
	
	protected static function getSingleRow($resource) {
		return mysql_fetch_assoc($resource);
	}
	
	protected static function getArray($resource) {
		$result = array();
		$rows = mysql_num_rows($resource);
		
		for($i = 0; $i < $rows; $i++) {
			$result[] = @mysql_fetch_assoc($resource);
		}
		
		return $result;
	}
	protected static function getInsertId() {
		return mysql_insert_id(Database::$connection);
	}
	public static function isDatabaseError() {
		return (get('file') == 'error' && get('id') &&
			(get('id') == ER_DATABASE_CONNECTION || get('id') == ER_DATABASE));
	}
}
?>