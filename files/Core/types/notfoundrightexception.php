<?php
class NotFoundRightException extends ViewException {
	public function getErrorFile() {
		return ERROR . DIRECTORY_SEPARATOR . 'NotFoudRight.inc';
	}
}