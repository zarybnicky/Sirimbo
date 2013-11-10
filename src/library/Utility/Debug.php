<?php
namespace TKOlomouc\Utility;

class Debug {
    function dump($var, $echo = false) {
        if ($echo) {
            var_export($var);
            return;
        }
        $this->log(var_export($var, true));
    }
    function dbg($string, $backtrace = true) {
        if ($backtrace) {
            $bt = debug_backtrace(DEBUG_BACKTRACE_IGNORE_ARGS, 2);
            $string = "{$bt[1]['file']}: {$bt[1]['line']} - $string";
        }
        $this->log($string);
    }
    private function log($string) {
        $out = fopen(DEBUG_LOG, 'a+');
        fwrite(
            $out,
            date(DATE_RFC822) . ":\n"
            . $string
            . "\n\n"
        );
        fclose($out);
    }
}
