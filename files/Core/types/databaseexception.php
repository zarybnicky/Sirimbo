<?php
class DatabaseException extends ViewException {
	public function getErrorFile() {
		return ERROR . DIRECTORY_SEPARATOR . 'Database.inc';
	}
}