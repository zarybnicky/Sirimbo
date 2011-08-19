<?php
class Log {
	private static $logfile;
	public static function write($message) {
		Log::$logfile = fopen(LOG, 'a');
		fwrite(Log::$logfile,  date(DATE_RFC822) . " - " . ($_GET ? $_GET["file"] : "'unknown'") .
			":\n\tMessage: $message" .
			"\n\tGET: " . json_encode($_GET) .
			"\n\tPOST: " . json_encode($_POST) .
			"\n\tSESSION: " . json_encode($_SESSION) . "\n");
		fclose(Log::$logfile);
	}
}
?>