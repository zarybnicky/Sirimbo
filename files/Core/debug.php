<?php
function dumpPost($echo = false) {
    if ($echo) {
        var_export($_POST);
        return;
    }
    $out = fopen(DEBUG_LOG, 'a');
    fwrite($out,  date(DATE_RFC822) . " - " .
        Request::getLiteralURI('home') . ":\n" .
        var_export($_POST, true) . "\n\n");
    fclose($out);
}
function dump($var, $echo = false) {
    if ($echo) {
        var_export($var);
        return;
    }
    $out = fopen(DEBUG_LOG, 'a');
    fwrite($out,  date(DATE_RFC822) . " - " .
        Request::getLiteralURI('home') . ":\n" .
        var_export($var, true) . "\n\n");
    fclose($out);
}
function dbg($string, $backtrace = true) {
    if ($backtrace) {
        $bt = debug_backtrace(DEBUG_BACKTRACE_IGNORE_ARGS, 2);
        $bt = $bt[1]['function'];
        $string = $bt[1]['file'] . ': ' . $bt[1]['line'] . ' - ' . $string;
    }

    $out = fopen(DEBUG_LOG, 'a');
    fwrite($out,  date(DATE_RFC822) . " - " .
        Request::getLiteralURI('home') . ":\n" .
        $string . "\n\n");
    fclose($out);
}
?>