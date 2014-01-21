<?php
namespace TKOlomouc\Utility;

class Request
{
    private static $rawUrlStatic;

    private $defaultSection = 'home';
    private $rawUrl;
    private $rawUrlParts;
    private $literalUrlParts;

    private $action;
    private $id;
    private $referer;

    private $view = 'html';

    public function setDefault($defaultSection)
    {
        $this->defaultSection = $defaultSection;
    }

    public function setURL($url)
    {
        list($url) = explode('?', $url);
        self::$rawUrlStatic = $url;
        $this->rawUrl = $url;
        $parts = explode('/', $url);

        //Removes double slashes
        foreach ($parts as $key => $part) {
            if ($part === '') {
                unset($parts[$key]);
            }
        }
        $parts = array_values($parts);
        $this->rawUrlParts = $parts;

        //Get an URL w/o numbers eg.
        foreach ($parts as $key => $part) {
            if (is_numeric($part)) {
                unset($parts[$key]);
            }
        }
        $parts = array_values($parts);
        $this->literalUrlParts = $parts;

        //Find controller action = the last string before a numerical one
        for ($i = count($this->rawUrlParts) - 1; $i >= 0; $i--) {
            if (!is_numeric($this->rawUrlParts[$i])) {
                continue;
            }
            $id = $this->rawUrlParts[$i];
            if (isset($this->rawUrlParts[$i - 1]) && !is_numeric($this->rawUrlParts[$i - 1])) {
                $_action = $this->rawUrlParts[$i - 1];
                break;
            }
        }
        $this->id = isset($id) ? $id : null;
        $this->action = isset($_action)
            ? $_action
            : (!empty($this->literalUrlParts)
                ? $this->literalUrlParts[count($this->literalUrlParts) - 1]
                : null);
    }

    public function getURL()
    {
        return $this->rawUrl;
    }

    public static function getURLStatic()
    {
        return self::$rawUrlStatic;
    }

    public function getRawURLParts()
    {
        return $this->rawUrlParts;
    }

    public function getLiteralURL()
    {
        if (empty($this->literalUrlParts) || $this->literalUrlParts[0] == '') {
            return $this->defaultSection;
        }
        return implode('/', $this->literalUrlParts);
    }

    public function getSection()
    {
        return isset($this->rawUrlParts[0]) ? $this->rawUrlParts[0] : $this->defaultSection;
    }

    public function getCanonical()
    {
        return $this->getLiteralURL();
    }

    public function getAction()
    {
        return $this->action;
    }

    public function getID()
    {
        return $this->id;
    }

    public function setReferer($referer)
    {
        $this->referer = $referer;
    }

    public function getReferer()
    {
        return $this->referer;
    }

    public function getView()
    {
        return $this->view;
    }

    public function setView($view = 'html')
    {
        $this->view = $view;
    }

    public function post($field, $value = null)
    {
        return $this->arrayManipulate($_POST, $field, $value);
    }

    public function get($field, $value = null)
    {
        return $this->arrayManipulate($_GET, $field, $value);
    }

    public function session($field, $value = null)
    {
        return $this->arrayManipulate($_SESSION, $field, $value);
    }

    public function server($field, $value = null)
    {
        return $this->arrayManipulate($_SERVER, $field, $value);
    }

    public function files($field, $value = null)
    {
        return $this->arrayManipulate($_FILES, $field, $value);
    }

    public function cookie($field, $value = null)
    {
        return $this->arrayManipulate($_COOKIE, $field, $value);
    }

    protected function arrayManipulate(&$array, $field = null, $value = null)
    {
        if ($field === null) {
            return $array;
        }
        if ($value !== null) {
            $array[$field] = $value;
        }
        if (isset($array[$field])) {
            return $array[$field];
        }
        return null;
    }
}
