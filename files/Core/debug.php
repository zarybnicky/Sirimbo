<?php
function echoPost() {
	if(DEBUG) {
		echo "<pre>";
		var_dump($_POST);
		echo "</pre>";
	}
}
function dumpVar($var) {
	if(DEBUG) {
		echo "<pre>";
		var_dump($var);
		echo "</pre>";
	}
}
function dbg($string) {
	if(DEBUG) {
		$bt = debug_backtrace();
		$bt = $bt[1]['function'];
		echo("DBG - $bt: $string<br/>\n");
	}
}
?>