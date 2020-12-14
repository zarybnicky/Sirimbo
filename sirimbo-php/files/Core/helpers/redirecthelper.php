<?php
class RedirectHelper
{
    public function __construct($link = null, $message = null, $type = null)
    {
        if ($message) {
            new \MessageHelper($type, $message);
        }
        if ($link) {
            header('Location: ' . $link);
            exit;
        }
        return $this;
    }
}
