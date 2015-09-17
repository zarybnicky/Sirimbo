<?php
class Log
{
    protected static $request;

    public static function setRequest($request)
    {
        static::$request = $request;
    }

    public static function write($message)
    {
        $logfile = fopen(LOG, 'a+');
        fwrite(
            $logfile,
            date(DATE_RFC822) . " - "
            . static::$request->server('REQUEST_URI') . ":\n"
            . "\tMessage: $message\n"
            . "\tGET: " . json_encode(static::$request->get()) . "\n"
            . "\tPOST: " . json_encode(static::$request->post()) . "\n"
            . "\tSESSION: " . json_encode(static::$request->session()) . "\n\n"
        );
        fclose($logfile);
    }
}
