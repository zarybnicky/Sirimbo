<?php
namespace TKOlomouc\Utility;

class Log
{
    private static $logfile;

    public static function write($message)
    {
        self::$logfile = fopen(LOG, 'a+');
        fwrite(self::$logfile,  date(DATE_RFC822) . ' - '
            . Request::getLiteralURL('home') . ":\n"
            . ($message ? "\tMessage: $message\n" : '')
            . "\tGET: " . json_encode($_GET) . "\n"
            . "\tPOST: " . json_encode($_POST) . "\n"
            . "\tSESSION: " . json_encode($_SESSION) . "\n\n");
        fclose(self::$logfile);
    }
}