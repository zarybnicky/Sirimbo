<?php
class KeyFileCorruptException extends ViewException {
	public function getErrorFile() {
		return ERROR . DIRECTORY_SEPARATOR . 'KeyFileCorrupt.inc';
	}
}