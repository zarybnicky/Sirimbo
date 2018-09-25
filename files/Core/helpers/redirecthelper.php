<?php
class RedirectHelper
{
    public function redirect($link = null, $message = null)
    {
        if ($link !== null) {
            $this->sendRedirect($link, $message);
        }
        return $this;
    }

    protected function sendRedirect($link, $message = '')
    {
        header('Location: ' . $link);

        if (isset($_SESSION['REDIRECT_MESSAGE'])) {
            $_SESSION['REDIRECT_MESSAGE'] .= '<br />' . $message;
        } elseif ($message) {
            $_SESSION['REDIRECT_MESSAGE'] = $message;
        }

        header('Location: ' . $link);
        exit;
    }

    public function setMessage($message)
    {
        if (is_array($message)) {
            foreach ($message as $row) {
                $this->setMessage($row);
            }
            return;
        }

        if (isset($_SESSION['REDIRECT_MESSAGE'])) {
            $_SESSION['REDIRECT_MESSAGE'] .= '<br />' . $message;
        } else {
            $_SESSION['REDIRECT_MESSAGE'] = $message;
        }
        return $this;
    }

    public function getMessage()
    {
        if (!isset($_SESSION['REDIRECT_MESSAGE'])) {
            return '';
        }

        $message = $_SESSION['REDIRECT_MESSAGE'];
        unset($_SESSION['REDIRECT_MESSAGE']);
        return $message;
    }
}
