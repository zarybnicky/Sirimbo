<?php
class Request
{
    private static $default = 'home';
    private static $uri;
    private static $url;
    private static $urlParts;
    private static $urlPartsLiteral;

    private static $action;
    private static $id;
    private static $referer;

    public static function setDefault($default) {
        Request::$default = $default;
    }
    public static function setURI($uri) {
        Request::$uri = $uri;
    }
    public static function setURL($url) {
        Request::$url = $url;
        $parts = explode('/', $url);

        foreach ($parts as $key => $part) {        //remove double slashes
            if ($part === '') {
                unset($parts[$key]);
            }
        }
        $parts = array_values($parts);
        Request::$urlParts = $parts;            //raw url parts

        foreach ($parts as $key => $part)
            if (is_numeric($part))
                unset($parts[$key]);            //make canonical name, usable for finding controllers

        $parts = array_values($parts);
        Request::$urlPartsLiteral = $parts;

        for($i = count(Request::$urlParts) - 1; $i >= 0; $i--) {
            if (is_numeric(Request::$urlParts[$i])) {
                $id = Request::$urlParts[$i];    //find last number, set it as the ID for most regular actions
                if (isset(Request::$urlParts[$i - 1]) && !is_numeric(Request::$urlParts[$i - 1])) {
                    $action = Request::$urlParts[$i - 1];        //dilemma: action before or after id???
                    break;
                }
            }
        }
        Request::$id = isset($id) ? $id : null;
        Request::$action = isset($action) ? $action :            //if previous code didn't find a viable action, use the last string in array
            (!empty(Request::$urlPartsLiteral) ?
                Request::$urlPartsLiteral[count(Request::$urlPartsLiteral) - 1] : null);
    }

    public static function getURI() {
        return Request::$uri;
    }
    public static function getURL() {
        return Request::$url;
    }
    public static function getURLParts() {
        return Request::$urlParts;
    }
    public static function getLiteralURL() {
        if (empty(Request::$urlPartsLiteral) || Request::$urlPartsLiteral[0] == '')
            return Request::$default;
        return implode('/', Request::$urlPartsLiteral);
    }
    public static function getSection() {
        return isset(Request::$urlParts[0]) ? Request::$urlParts[0] : Request::$default;
    }
    public static function getCanonical() {
        return Request::getLiteralURL();
    }
    public static function getAction() {
        return Request::$action;
    }
    public static function getID() {
        return Request::$id;
    }

    public static function setReferer($referer) {
        Request::$referer = $referer;
    }
    public static function getReferer() {
        return Request::$referer;
    }
}
?>