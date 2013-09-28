<?php
class AuthorizationException extends ViewException {
	public function getErrorFile() {
		return ERROR . DIRECTORY_SEPARATOR . 'Authorization.inc';
	}
}