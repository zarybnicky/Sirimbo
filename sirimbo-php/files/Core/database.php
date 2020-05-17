<?php
class Database
{
    protected static $connection;
    public static $request;

    public static function setRequest($request)
    {
        static::$request = $request;
    }

    protected static function escapeArray($array)
    {
        $escaped = [];
        foreach ($array as $key => $value) {
            if (is_array($value)) {
                $escaped[$key] = self::escapeArray($value);
                unset($array[$key]);
            }
        }
        if ($array) {
            $escape = implode("%%%%%", $array);
            $escape = static::getConnection()->real_escape_string($escape);
            $array = explode("%%%%%", $escape);
        }
        if ($escaped) {
            /** @var int $key */
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
        if (self::$connection == null) {
            self::$connection = new mysqli(DB_SERVER, DB_USER, DB_PASS, DB_DATABASE);
            if (self::$connection->error) {
                static::databaseError(true);
            }
            self::$connection->set_charset("utf8");
        }
        return self::$connection;
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
        $res = static::getConnection()->query($query);
        if (!$res) {
            static::databaseError();
        }
        return $res;
    }

    protected static function getSingleRow($resource)
    {
        return $resource ? $resource->fetch_assoc() : false;
    }

    protected static function getArray($resource)
    {
        $result = [];
        while ($row = $resource->fetch_assoc()) {
            $result[] = $row;
        }
        return $result;
    }

    public static function getInsertId()
    {
        return self::$connection->insert_id;
    }

    protected static function databaseError($onConnection = false)
    {
        Log::write('MySQL Error: ' . self::$connection->errno . ': ' . self::$connection->error);
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
