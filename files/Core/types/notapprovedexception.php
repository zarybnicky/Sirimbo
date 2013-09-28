<?php
class NotApprovedException extends ViewException {
	public function getErrorFile() {
		return ERROR . DIRECTORY_SEPARATOR . 'NotApproved.inc';
	}
}