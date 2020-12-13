<?php
class RedirectHelper
{
    public function __construct($link = null, $message = null, $type = 'info')
    {
        if ($message) {
            $this->setMessage($message, $type);
        }
        if ($link) {
            header('Location: ' . $link);
            exit;
        }
        return $this;
    }

    public function setMessage($message, $type = 'info')
    {
        if (!isset($_SESSION['REDIRECT_MESSAGE'])) {
            $_SESSION['REDIRECT_MESSAGE'] = [];
        }
        if (!is_array($message)) {
            $_SESSION['REDIRECT_MESSAGE'][] = [$message, $type];
            return $this;
        }
        foreach ($message as $item) {
            if (is_array($item)) {
                $_SESSION['REDIRECT_MESSAGE'][] = $item;
            } else {
                $_SESSION['REDIRECT_MESSAGE'][] = [$item, $type];
            }
        }
        return $this;
    }

    public function primary($message)
    {
        $this->setMessage($message, 'primary');
    }

    public function secondary($message)
    {
        $this->setMessage($message, 'secondary');
    }

    public function success($message)
    {
        $this->setMessage($message, 'success');
    }

    public function danger($message)
    {
        $this->setMessage($message, 'danger');
    }

    public function warning($message)
    {
        $this->setMessage($message, 'warning');
    }

    public function info($message)
    {
        $this->setMessage($message, 'info');
    }

    public function getMessages()
    {
        if (!isset($_SESSION['REDIRECT_MESSAGE'])) {
            return [];
        }

        $message = $_SESSION['REDIRECT_MESSAGE'];
        $_SESSION['REDIRECT_MESSAGE'] = [];
        return $message;
    }
}
