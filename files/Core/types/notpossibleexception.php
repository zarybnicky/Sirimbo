<?php
class NotPossibleException extends ViewException {
	public function getErrorFile() {
		return ERROR . DIRECTORY_SEPARATOR . 'NotPossible.inc';
	}
}