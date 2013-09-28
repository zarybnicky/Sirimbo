<?php
class ViewException extends Exception {
	public function __construct($message, $code = 0, Exception $previous = null) {
		parent::__construct($message, $code, $previous);
	}
	public function getErrorFile() {
		return ERROR . DIRECTORY_SEPARATOR . 'ScriptFatal.inc';
	}
}