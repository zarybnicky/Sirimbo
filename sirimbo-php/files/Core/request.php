<?php
class Request
{
    protected $uri;
    protected $method;

    protected $rawUriParts;

    protected $action;
    protected $id;

    public function __construct($uri, $method)
    {
        $this->method = $method;

        $uri = explode('?', $uri)[0];
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

        //Find controller action = the last string before a numerical one
        for ($i = count($this->rawUriParts) - 1; $i >= 0; $i--) {
            if (!is_numeric($this->rawUriParts[$i])) {
                continue;
            }
            $id = $this->rawUriParts[$i];
            if (isset($this->rawUriParts[$i - 1]) && !is_numeric($this->rawUriParts[$i - 1])) {
                $action = $this->rawUriParts[$i - 1];
                break;
            }
        }
        $this->id = isset($id) ? $id : null;
        $this->action = str_replace(
            '-',
            '_',
            isset($action) ? $action : (!empty($parts) ? $parts[count($parts) - 1] : '')
        );
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
