<?php
class Log
{
    private static $_logfile;

    public static function write($message)
    {
        Log::$_logfile = fopen(LOG, 'a+');
        fwrite(Log::$_logfile,  date(DATE_RFC822) . " - " .
            Request::getLiteralURL('home') . ":\n" .
            ($message ? "\tMessage: $message\n" : '') .
            "\tGET: " . json_encode($_GET) . "\n" .
            "\tPOST: " . json_encode($_POST) . "\n" .
            "\tSESSION: " . json_encode($_SESSION) . "\n\n");
        fclose(Log::$_logfile);
    }
}
?>