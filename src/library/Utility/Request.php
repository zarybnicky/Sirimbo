<?php
namespace TKOlomouc\Utility;

class Request
{
    private static $_default = 'home';
    private static $_url;
    private static $_rawUrlParts;
    private static $_urlPartsLiteral;

    private static $_action;
    private static $_id;
    private static $_referer;

    public static function setDefault($_default)
    {
        self::$_default = $_default;
    }
    public static function setURL($_url)
    {
        self::$_url = $_url;
        $parts = explode('/', $_url);

        //Removes double slashes
        foreach ($parts as $key => $part) {
            if ($part === '') {
                unset($parts[$key]);
            }
        }
        $parts = array_values($parts);
        self::$_rawUrlParts = $parts;

        //Get an URL w/o numbers eg.
        foreach ($parts as $key => $part) {
            if (is_numeric($part)) {
                unset($parts[$key]);
            }
        }
        $parts = array_values($parts);
        self::$_urlPartsLiteral = $parts;

        //Find controller action = the last string before a numerical one
        for($i = count(self::$_rawUrlParts) - 1; $i >= 0; $i--) {
            if (!is_numeric(self::$_rawUrlParts[$i])) {
                continue;
            }
            $_id = self::$_rawUrlParts[$i];
            if (isset(self::$_rawUrlParts[$i - 1]) && !is_numeric(self::$_rawUrlParts[$i - 1])) {
                $_action = self::$_rawUrlParts[$i - 1];
                break;
            }
        }
        self::$_id = isset($_id) ? $_id : null;
        self::$_action = isset($_action)
            ? $_action
            : (!empty(self::$_urlPartsLiteral)
                ? self::$_urlPartsLiteral[count(self::$_urlPartsLiteral) - 1]
                : null);
    }

    public static function getURL()
    {
        return self::$_url;
    }
    public static function getRawURLParts()
    {
        return self::$_rawUrlParts;
    }
    public static function getLiteralURL()
    {
        if (empty(self::$_urlPartsLiteral) || self::$_urlPartsLiteral[0] == '') {
            return self::$_default;
        }
        return implode('/', self::$_urlPartsLiteral);
    }
    public static function getSection()
    {
        return isset(self::$_rawUrlParts[0]) ? self::$_rawUrlParts[0] : self::$_default;
    }
    public static function getCanonical()
    {
        return self::getLiteralURL();
    }
    public static function getAction()
    {
        return self::$_action;
    }
    public static function getID()
    {
        return self::$_id;
    }

    public static function setReferer($referer)
    {
        self::$_referer = $referer;
    }
    public static function getReferer()
    {
        return self::$_referer;
    }
}
?>