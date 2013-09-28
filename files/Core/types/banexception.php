<?php
class BanException extends ViewException {
	public function getErrorFile() {
		return ERROR . DIRECTORY_SEPARATOR . 'Ban.inc';
	}
}