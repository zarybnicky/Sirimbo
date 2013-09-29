<?php
class DatabaseException extends ViewException {
	public function getErrorFile() {
		return 'database';
	}
}