<?php
class Request
{
    protected $uri;
    protected $method;
    protected $headers;
    protected $serverParams;
    protected $cookieParams;
    protected $getParams;
    protected $postParams;
    protected $fileParams;
    protected $sessionParams;

    protected $defaultPath;
    protected $rawUriParts;
    protected $uriPartsLiteral;

    protected $action;
    protected $id;
    protected $referer;

    public function __construct(
        $uri,
        $method,
        $headers,
        $serverParams,
        $cookieParams,
        $getParams,
        $postParams,
        $fileParams,
        $sessionParams
    ) {
        $this->setURI($uri);
        $this->method = $method;
        $this->headers = $headers;
        $this->serverParams = $serverParams;
        $this->cookieParams = $cookieParams;
        $this->getParams = $getParams;
        $this->postParams = $postParams;
        $this->fileParams = $fileParams;
        $this->sessionParams = $sessionParams;
    }

    protected function phpGlobal(&$array, $field, $value)
    {
        if ($field === null) {
            return $array;
        }

        if ($value !== null) {
            $array[$field] = $value;
            return;
        }

        if (isset($array[$field])) {
            return $array[$field];
        } else {
            return null;
        }
    }

    public function server($field = null, $value = null)
    {
        return $this->phpGlobal($this->serverParams, $field, $value);
    }

    public function cookie($field = null, $value = null)
    {
        return $this->phpGlobal($this->cookieParams, $field, $value);
    }

    public function get($field = null, $value = null)
    {
        return $this->phpGlobal($this->getParams, $field, $value);
    }

    public function post($field = null, $value = null)
    {
        return $this->phpGlobal($this->postParams, $field, $value);
    }

    public function files($field = null, $value = null)
    {
        return $this->phpGlobal($this->fileParams, $field, $value);
    }

    public function session($field = null, $value = null)
    {
        if ($value !== null) {
            $_SESSION[$field] = $value;
        }
        return $this->phpGlobal($this->sessionParams, $field, $value);
    }

    public function setDefault($defaultPath)
    {
        $this->defaultPath = $defaultPath;
    }

    public function setURI($uri)
    {
        $uri = explode('?', $uri)[0];

        $this->uri = trim($uri, '/');
        $parts = explode('/', $uri);

        //Removes double slashes
        foreach ($parts as $key => $part) {
            if ($part === '') {
                unset($parts[$key]);
            }
        }
        $parts = array_values($parts);
        $this->rawUriParts = $parts;

        //Get an URI w/o numbers eg.
        foreach ($parts as $key => $part) {
            if (is_numeric($part)) {
                unset($parts[$key]);
            }
        }
        $parts = array_values($parts);
        $this->uriPartsLiteral = $parts;

        //Find controller action = the last string before a numerical one
        for ($i = count($this->rawUriParts) - 1; $i >= 0; $i--) {
            if (!is_numeric($this->rawUriParts[$i])) {
                continue;
            }
            $id = $this->rawUriParts[$i];
            if (isset($this->rawUriParts[$i - 1]) &&
                !is_numeric($this->rawUriParts[$i - 1])
            ) {
                $action = $this->rawUriParts[$i - 1];
                break;
            }
        }
        $this->id = isset($id) ? $id : null;
        $this->action = isset($action)
                      ? $action
                      : (!empty($this->uriPartsLiteral)
                         ? $this->uriPartsLiteral[count($this->uriPartsLiteral) - 1]
                         : null);
        $this->action = str_replace('-', '_', $this->action);
    }

    public function getURI()
    {
        return $this->uri;
    }

    public function getLiteralURI()
    {
        if (empty($this->uriPartsLiteral) || $this->uriPartsLiteral[0] == '') {
            return $this->defaultPath;
        }
        return implode('/', $this->uriPartsLiteral);
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
}
