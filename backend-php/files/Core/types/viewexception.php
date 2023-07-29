<?php
class ViewException extends Exception
{
    public $errorFile;

    public function __construct($message, $errorFile = 'script_fatal', $code = 0, Exception $previous = null) {
        parent::__construct($message, $code, $previous);
        $this->errorFile = $errorFile;
    }

    public function getErrorFile() {
        return $this->errorFile;
    }
}
