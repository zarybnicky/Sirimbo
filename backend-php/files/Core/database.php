<?php
class Database
{
    protected static $connection;
    protected static $tr = [
        "\0" => "\\0",
        "\x08" => "\\b",
        "\x1a" => "\\z",
        "'" => "''",
    ];

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
            $escape = strtr($escape, self::$tr);
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
            self::$connection = new PDO(DB_CONN_STRING, null, null, [PDO::ATTR_ERRMODE => PDO::ERRMODE_EXCEPTION]);
        }
        return self::$connection;
    }

    protected static function prepare($query)
    {
        syslog(LOG_ERR, str_replace(["\n", "\r"], '', $query));
        return static::getConnection()->prepare($query);
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
        return static::getConnection()->query($query);
    }

    protected static function getSingleRow($resource)
    {
        return $resource ? $resource->fetch(PDO::FETCH_ASSOC) : false;
    }

    protected static function getArray($resource)
    {
        return $resource->fetchAll(PDO::FETCH_ASSOC);
    }

    public static function getInsertId()
    {
        return self::$connection->lastInsertId();
    }
}