<?php
class CorruptDataException extends ViewException {
	public function getErrorFile() {
		return ERROR . DIRECTORY_SEPARATOR . 'CorruptData.inc';
	}
}