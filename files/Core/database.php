<?php
class Database
{
    protected static $connection;
    protected static $request;

    public static function getInstance()
    {
        return new self();
    }

    public static function setRequest($request)
    {
        static::$request = $request;
    }

    protected static function escapeArray($array)
    {
        static::getConnection();
        $escaped = array();
        foreach ($array as $key => $value) {
            if (is_array($value)) {
                $escaped[$key] = self::escapeArray($value);
                unset($array[$key]);
            }
        }
        if ($array) {
            $escape = implode("%%%%%", $array);
            $escape = mysql_real_escape_string($escape);
            $array = explode("%%%%%", $escape);
        }
        if ($escaped) {
            foreach ($escaped as $key => $value) {
                array_splice($array, $key, 0, array($value));
            }
        }
        return $array;
    }
    protected static function escape($string)
    {
        return self::escapeArray(func_get_args());
    }
    protected static function getConnection()
    {
        if (self::$connection != null) {
            return;
        }
        self::$connection = @mysql_connect(DB_SERVER, DB_USER, DB_PASS)
            or static::databaseError(true);
        @mysql_select_db(DB_DATABASE, self::$connection)
            or static::databaseError(true);
        @mysql_set_charset("utf8", self::$connection)
            or static::databaseError(true);
    }
    protected static function query($query)
    {
        static::getConnection();
        $res = mysql_query($query, self::$connection)
            or static::databaseError();

        return $res;
    }
    protected static function getSingleRow($resource)
    {
        return mysql_fetch_assoc($resource);
    }

    protected static function getArray($resource)
    {
        $result = array();
        $rows = mysql_num_rows($resource);

        for($i = 0; $i < $rows; $i++) {
            $result[] = @mysql_fetch_assoc($resource);
        }
        return $result;
    }
    public static function getInsertId()
    {
        return mysql_insert_id(self::$connection);
    }
    protected function databaseError($onConnection = false)
    {
        Log::write('MySQL Error: ' . mysql_errno() . ': ' . mysql_error());
        if ($onConnection) {
            throw new DatabaseConnectionException('Nastala chyba při pokusu o připojení k databázi.');
        } else {
            throw new DatabaseException('Nastala chyba v dotazu na databázi.');
        }
    }
    public static function isDatabaseError()
    {
        return (
            static::$request->get('file') == 'error' &&
            static::$request->get('id') &&
            stripos(static::$request->get('id'), 'database') !== null
        );
    }
}
