<?php
class Request
{
    protected $uri;
    protected $method;
    protected $getParams;
    protected $postParams;

    protected $defaultPath;
    protected $rawUriParts;
    protected $uriPartsLiteral;

    protected $action;
    protected $id;

    public function __construct(
        $uri,
        $method,
        $getParams,
        $postParams
    ) {
        $this->setURI($uri);
        $this->method = $method;
        $this->getParams = $getParams;
        $this->postParams = $postParams;
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

    public function get($field = null, $value = null)
    {
        return $this->phpGlobal($this->getParams, $field, $value);
    }

    public function post($field = null, $value = null)
    {
        return $this->phpGlobal($this->postParams, $field, $value);
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
        $this->action = str_replace(
            '-',
            '_',
            isset($action) ? $action :
            (!empty($this->uriPartsLiteral)
             ? $this->uriPartsLiteral[count($this->uriPartsLiteral) - 1]
             : '')
        );
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
}
