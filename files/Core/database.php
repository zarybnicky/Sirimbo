<?php
class Database {
	private static $connection = null;

	public static function getInstance() { return new self(); }
	
	protected static function escapeArray($array) {
		Database::getConnection();
		$escaped = array();
		foreach($array as $key => $value) {
			if(is_array($value)) {
				$escaped[$key] = self::escapeArray($value);
				unset($array[$key]);
			}
		}
		if($array) {
			$escape = implode("%%%%%", $array);
			$escape = mysql_real_escape_string($escape);
			$array = explode("%%%%%", $escape);
		}
		if($escaped) {
			foreach($escaped as $key => $value)
				array_splice($array, $key, 0, array($value));
		}
		return $array;
	}
	protected static function escape($string) {
		return self::escapeArray(func_get_args());
	}
	
	protected static function getConnection() {
		if(Database::$connection != null)
			return;
		Database::$connection = @mysql_connect(DB_SERVER, DB_USER, DB_PASS)
			or static::databaseError(true);
		@mysql_select_db(DB_DATABASE, Database::$connection)
			or static::databaseError(true);
		@mysql_set_charset("utf8", Database::$connection)
			or static::databaseError(true);
	}
	
	protected static function query($query) {
		Database::getConnection();
		$res = mysql_query($query, Database::$connection)
			or static::databaseError();
			
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
	public static function getInsertId() {
		return mysql_insert_id(Database::$connection);
	}
	protected function databaseError($onConnection = false) {
		Log::write('MySQL Error: ' . mysql_errno() . ': ' . mysql_error());
		if($onConnection)
			throw new DatabaseConenctionException('Nastala chyba při pokusu o připojení k databázi.');
		else
			throw new DatabaseException('Nastala chyba v dotazu na databázi.');
	}
	public static function isDatabaseError() {
		return (get('file') == 'error' && get('id') &&
			(get('id') == ER_DATABASE_CONNECTION || get('id') == ER_DATABASE));
	}
}
?>