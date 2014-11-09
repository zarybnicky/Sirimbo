<?php
class Request
{
    private static $_default = 'home';
    private static $_uri;
    private static $_rawUriParts;
    private static $_uriPartsLiteral;

    private static $_action;
    private static $_id;
    private static $_referer;

    public static function setDefault($_default) {
        self::$_default = $_default;
    }
    public static function setURI($_uri) {
        $_uri = explode('?', $_uri)[0];

        self::$_uri = trim($_uri, '/');
        $parts = explode('/', $_uri);

        //Removes double slashes
        foreach ($parts as $key => $part) {
            if ($part === '') {
                unset($parts[$key]);
            }
        }
        $parts = array_values($parts);
        self::$_rawUriParts = $parts;

        //Get an URI w/o numbers eg.
        foreach ($parts as $key => $part) {
            if (is_numeric($part)) {
                unset($parts[$key]);
            }
        }
        $parts = array_values($parts);
        self::$_uriPartsLiteral = $parts;

        //Find controller action = the last string before a numerical one
        for($i = count(self::$_rawUriParts) - 1; $i >= 0; $i--) {
            if (!is_numeric(self::$_rawUriParts[$i])) {
                continue;
            }
            $_id = self::$_rawUriParts[$i];
            if (isset(self::$_rawUriParts[$i - 1]) && !is_numeric(self::$_rawUriParts[$i - 1])) {
                $_action = self::$_rawUriParts[$i - 1];
                break;
            }
        }
        self::$_id = isset($_id) ? $_id : null;
        self::$_action = isset($_action)
            ? $_action
            : (!empty(self::$_uriPartsLiteral)
                ? self::$_uriPartsLiteral[count(self::$_uriPartsLiteral) - 1]
                : null);
        self::$_action = str_replace('-', '_', self::$_action);
    }

    public static function getURI() {
        return self::$_uri;
    }
    public static function getRawURIParts() {
        return self::$_rawUriParts;
    }
    public static function getLiteralURI() {
        if (empty(self::$_uriPartsLiteral) || self::$_uriPartsLiteral[0] == '') {
            return self::$_default;
        }
        return implode('/', self::$_uriPartsLiteral);
    }
    public static function getSection() {
        return isset(self::$_rawUriParts[0]) ? self::$_rawUriParts[0] : self::$_default;
    }
    public static function getCanonical() {
        return self::getLiteralURI();
    }
    public static function getAction() {
        return self::$_action;
    }
    public static function getID() {
        return self::$_id;
    }

    public static function setReferer($referer) {
        self::$_referer = $referer;
    }
    public static function getReferer() {
        return self::$_referer;
    }
}
