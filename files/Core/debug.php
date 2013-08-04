<?php
function dumpPost() {
	$out = fopen(DEBUG_LOG, 'a+');
	fwrite($out,  date(DATE_RFC822) . " - " .
		Request::getLiteralURL('home') . ":\n" .
		var_export($_POST, true) . "\n\n");
	fclose($out);
}
function dump($var) {
	$out = fopen(DEBUG_LOG, 'a+');
	fwrite($out,  date(DATE_RFC822) . " - " .
		Request::getLiteralURL('home') . ":\n" .
		var_export($var, true) . "\n\n");
	fclose($out);
}
function dbg($string, $backtrace = true) {
	if($backtrace) {
		$bt = debug_backtrace(DEBUG_BACKTRACE_IGNORE_ARGS, 2);
		$bt = $bt[1]['function'];
		$string = $bt[1]['file'] . ': ' . $bt[1]['line'] . ' - ' . $string;
	}
	
	$out = fopen(DEBUG_LOG, 'a+');
	fwrite($out,  date(DATE_RFC822) . " - " .
		Request::getLiteralURL('home') . ":\n" .
		$string . "\n\n");
	fclose($out);
}
?>