<?php
class Redirect
{
    public static function to($link)
    {
        header('Location: ' . $link);
        exit;
    }
}
