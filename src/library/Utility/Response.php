<?php
namespace TKOlomouc\Utility;

class Response
{
    public static function redirect($link, $message = null, $overwrite = false)
    {
        header('Location: ' . $link);

        if ($message !== null) {
            self::setMessage($message, $overwrite);
        }
        exit;
    }

    public static function setMessage($message, $overwrite = false)
    {
        if ($overwrite) {
            unset($_SESSION['REDIRECT_MESSAGE']);
        }

        if (is_array($message)) {
            foreach ($message as $row) {
                self::setMessage($row);
            }
            return;
        }

        if (isset($_SESSION['REDIRECT_MESSAGE'])) {
            $_SESSION['REDIRECT_MESSAGE'] .= '<br />' . $message;
        } else {
            $_SESSION['REDIRECT_MESSAGE'] = $message;
        }
    }

    public static function getMessage()
    {
        if (!isset($_SESSION['REDIRECT_MESSAGE'])) {
            return '';
        }

        $message = $_SESSION['REDIRECT_MESSAGE'];

        unset($_SESSION['REDIRECT_MESSAGE']);

        return $message;
    }
}