<?php
namespace TKOlomouc\Model\Database;

use TKOlomouc\Utility\Log;
use TKOlomouc\View\Exception\DatabaseConnectionException;
use TKOlomouc\View\Exception\DatabaseException;
use TKOlomouc\Utility\Debug;
use TKOlomouc\Utility\Request;

class Adapter
{
    private static $connection = null;

    public static function getInstance()
    {
        return new self();
    }

    protected static function escapeArray($array)
    {
        self::getConnection();
        $escaped = array();
        foreach ($array as $key => $value) {
            if (is_array($value)) {
                $escaped[$key] = self::escapeArray($value);
                unset($array[$key]);
            }
        }
        if ($array) {
            $escape = implode("%%%%%", $array);
            $escape = self::$connection->quote($escape);
            $escape = substr($escape, 1, strlen($escape) - 2);
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
        self::$connection = new \PDO(DB_PDO, DB_USER, DB_PASS);
        self::$connection->setAttribute(\PDO::ATTR_ERRMODE, \PDO::ERRMODE_EXCEPTION);
        self::$connection->exec("SET NAMES utf8");
    }

    protected static function query($query)
    {
        self::getConnection();
        return self::$connection->query($query);
    }

    protected static function getSingleRow(\PDOStatement $resource)
    {
        return $resource->fetch(\PDO::FETCH_ASSOC);
    }

    protected static function getArray(\PDOStatement $resource)
    {
        return $resource->fetchAll(\PDO::FETCH_ASSOC);
    }

    public static function getInsertId()
    {
        return self::$connection->lastInsertId();
    }

    protected function databaseError($onConnection = false)
    {
        Log::write('MySQL Error: ' . implode(', ', self::$connection->errorInfo()));
        if ($onConnection) {
            throw new DatabaseConnectionException('Nastala chyba při pokusu o připojení k databázi.');
        } else {
            throw new DatabaseException('Nastala chyba v dotazu na databázi.');
        }
    }

    public static function isDatabaseError(Request $request)
    {
        return ($request->getAction() == 'error'
            && ($request->getID() == ER_DATABASE_CONNECTION || $request->getID() == ER_DATABASE));
    }
}
