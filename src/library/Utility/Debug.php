<?php
namespace TKOlomouc\Utility;

class Debug
{
    public static function dump($var, $echo = false)
    {
        if ($echo) {
            var_export($var);
            return;
        }
        self::log(var_export($var, true));
    }

    public static function traceback($string, $backtrace = true)
    {
        if ($backtrace) {
            $bt = debug_backtrace(DEBUG_BACKTRACE_IGNORE_ARGS, 2);
            $string = "{$bt[1]['file']}: {$bt[1]['line']} - $string";
        }
        self::log($string);
    }

    private static function log($string)
    {
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
