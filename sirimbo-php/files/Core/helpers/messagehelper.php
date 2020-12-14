<?php
class MessageHelper
{
    // types = primary, secondary, success, danger, warning, info
    public function __construct($type, $message = null)
    {
        if ($message === null) {
            $message = $type;
            $type = 'info';
        }
        if (!isset($_SESSION['REDIRECT_MESSAGE'])) {
            $_SESSION['REDIRECT_MESSAGE'] = [];
        }
        if (!is_array($message)) {
            $_SESSION['REDIRECT_MESSAGE'][] = [$message, $type];
            return;
        }
        foreach ($message as $item) {
            if (is_array($item)) {
                $_SESSION['REDIRECT_MESSAGE'][] = $item;
            } else {
                $_SESSION['REDIRECT_MESSAGE'][] = [$item, $type];
            }
        }
    }

    public static function getMessages()
    {
        if (!isset($_SESSION['REDIRECT_MESSAGE'])) {
            return [];
        }

        $message = $_SESSION['REDIRECT_MESSAGE'];
        $_SESSION['REDIRECT_MESSAGE'] = [];
        return $message;
    }
}
