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
        $escaped = [];
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
                array_splice($array, $key, 0, [$value]);
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
        self::$connection = mysql_connect(DB_SERVER, DB_USER, DB_PASS)
            or static::databaseError(true);
        mysql_select_db(DB_DATABASE, self::$connection)
            or static::databaseError(true);
        mysql_set_charset("utf8", self::$connection)
            or static::databaseError(true);
    }
    protected static function query($query)
    {
        if (func_num_args() > 1) {
            $args = static::escapeArray(array_slice(func_get_args(), 1));
            $q = '';
            while (strpos($query, '?') !== false) {
                if (!$args) {
                    throw new ViewException('Invalid query call');
                }
                list($x, $rest) = explode('?', $query, 2);
                $q .= $x . array_shift($args);
                $query = $rest;
            }
            if ($args) {
                throw new ViewException('Invalid query call');
            }
            $query = $q . $query;
        }
        static::getConnection();
        $res = mysql_query($query, self::$connection);
        if (!$res) {
            static::databaseError();
        }
        return $res;
    }
    protected static function getSingleRow($resource)
    {
        return $resource ? mysql_fetch_assoc($resource) : false;
    }

    protected static function getArray($resource)
    {
        $result = [];
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
