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

    public static function setMessage(string $type, string $message): void
    {
        if (!isset($_SESSION['REDIRECT_MESSAGE'])) {
            $_SESSION['REDIRECT_MESSAGE'] = [];
        }
        $_SESSION['REDIRECT_MESSAGE'][] = ['text' => $message, 'type' => $type];
    }

    public static function info(string $msg): void
    {
        self::setMessage('info', $msg);
    }
    public static function warning(string $msg): void
    {
        self::setMessage('warning', $msg);
    }
    public static function danger(string $msg): void
    {
        self::setMessage('danger', $msg);
    }
    public static function success(string $msg): void
    {
        self::setMessage('success', $msg);
    }
    public static function primary(string $msg): void
    {
        self::setMessage('primary', $msg);
    }
    public static function secondary(string $msg): void
    {
        self::setMessage('secondary', $msg);
    }
}
