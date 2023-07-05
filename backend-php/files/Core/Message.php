<?php
class Message
{
    public static function get(): array
    {
        if (!isset($_SESSION['REDIRECT_MESSAGE'])) {
            return [];
        }

        $message = $_SESSION['REDIRECT_MESSAGE'];
        $_SESSION['REDIRECT_MESSAGE'] = [];
        return $message;
    }

    public static function setMessage(string $type, $message): void
    {
        if (!isset($_SESSION['REDIRECT_MESSAGE'])) {
            $_SESSION['REDIRECT_MESSAGE'] = [];
        }
        if (is_array($message)) {
            foreach ($message as $item) {
                $_SESSION['REDIRECT_MESSAGE'][] = ['text' => $item, 'type' => $type];
            }
        } else {
            $_SESSION['REDIRECT_MESSAGE'][] = ['text' => $message, 'type' => $type];
        }
    }

    public static function info($msg): void
    {
        self::setMessage('info', $msg);
    }
    public static function warning($msg): void
    {
        self::setMessage('warning', $msg);
    }
    public static function danger($msg): void
    {
        self::setMessage('danger', $msg);
    }
    public static function success($msg): void
    {
        self::setMessage('success', $msg);
    }
}
