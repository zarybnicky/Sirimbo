<?php
class DatabaseConenctionException extends ViewException {
	public function getErrorFile() {
		return ERROR . DIRECTORY_SEPARATOR . 'DatabaseConnection.inc';
	}
}