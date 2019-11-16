<?php
function dump($var, $echo = false)
{
    if ($echo) {
        var_export($var);
        return;
    }
    if ($out = fopen(DEBUG_LOG, 'a')) {
        fwrite($out, date(DATE_RFC822) . "\n" . var_export($var, true) . "\n\n");
        fclose($out);
    }
}
function dbg($string, $backtrace = true)
{
    if ($backtrace) {
        $bt = debug_backtrace(DEBUG_BACKTRACE_IGNORE_ARGS, 2);
        $bt = $bt[1]['function'];
        $string = $bt[1]['file'] . ': ' . $bt[1]['line'] . "\n" . $string;
    }

    if ($out = fopen(DEBUG_LOG, 'a')) {
        fwrite($out, date(DATE_RFC822) . "\n" . $string . "\n\n");
        fclose($out);
    }
}
