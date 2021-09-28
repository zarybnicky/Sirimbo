<?php
class Database
{
    protected static $connection;

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
        syslog(LOG_ERR, str_replace(["\n", "\r"], '', $query));
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
        $msg = 'MySQL Error: ' . self::$connection->errno . ': ' . self::$connection->error;
        if ($onConnection) {
            throw new DatabaseConnectionException($msg);
        } else {
            throw new DatabaseException($msg);
        }
    }

    public static function isDatabaseError()
    {
        return ($_GET['file'] ?? '') == 'error' && stripos($_GET['id'] ?? '', 'database') !== null;
    }
}
