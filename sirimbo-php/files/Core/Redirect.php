<?php
class Redirect
{
    public static function to($link, $message = null, $type = null)
    {
        if ($message) {
            new \MessageHelper($type, $message);
        }
        header('Location: ' . $link);
        exit;
    }
}
