<?php
namespace TKOlomouc\Utility;

class Request
{
    private static $defaultSection = 'home';
    private static $rawUrl;
    private static $rawUrlParts;
    private static $literalUrlParts;

    private static $action;
    private static $id;
    private static $referer;

    public static function setDefault($defaultSection)
    {
        self::$defaultSection = $defaultSection;
    }

    public static function setURL($url)
    {
        self::$rawUrl = $url;
        $parts = explode('/', $url);

        //Removes double slashes
        foreach ($parts as $key => $part) {
            if ($part === '') {
                unset($parts[$key]);
            }
        }
        $parts = array_values($parts);
        self::$rawUrlParts = $parts;

        //Get an URL w/o numbers eg.
        foreach ($parts as $key => $part) {
            if (is_numeric($part)) {
                unset($parts[$key]);
            }
        }
        $parts = array_values($parts);
        self::$literalUrlParts = $parts;

        //Find controller action = the last string before a numerical one
        for($i = count(self::$rawUrlParts) - 1; $i >= 0; $i--) {
            if (!is_numeric(self::$rawUrlParts[$i])) {
                continue;
            }
            $id = self::$rawUrlParts[$i];
            if (isset(self::$rawUrlParts[$i - 1]) && !is_numeric(self::$rawUrlParts[$i - 1])) {
                $_action = self::$rawUrlParts[$i - 1];
                break;
            }
        }
        self::$id = isset($id) ? $id : null;
        self::$action = isset($_action)
            ? $_action
            : (!empty(self::$literalUrlParts)
                ? self::$literalUrlParts[count(self::$literalUrlParts) - 1]
                : null);
    }

    public static function getURL()
    {
        return self::$rawUrl;
    }

    public static function getRawURLParts()
    {
        return self::$rawUrlParts;
    }

    public static function getLiteralURL()
    {
        if (empty(self::$literalUrlParts) || self::$literalUrlParts[0] == '') {
            return self::$defaultSection;
        }
        return implode('/', self::$literalUrlParts);
    }

    public static function getSection()
    {
        return isset(self::$rawUrlParts[0]) ? self::$rawUrlParts[0] : self::$defaultSection;
    }

    public static function getCanonical()
    {
        return self::getLiteralURL();
    }

    public static function getAction()
    {
        return self::$action;
    }

    public static function getID()
    {
        return self::$id;
    }

    public static function setReferer($referer)
    {
        self::$referer = $referer;
    }

    public static function getReferer()
    {
        return self::$referer;
    }
}
