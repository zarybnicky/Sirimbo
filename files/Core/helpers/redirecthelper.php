<?php
class RedirectHelper {
    public function redirect($link = null, $message = null, $replaceMessage = false) {
        if($link !== null)
            $this->sendRedirect($link, $message, $replaceMessage);        
        return $this;
    }
    public static function sendRedirect($link, $message = '', $replaceMessage = false) {
        header('Location: ' . $link);
    
        if(!$message)
            exit;
        if(isset($_SESSION['REDIRECT_MESSAGE']) && $replaceMessage == false)
            $_SESSION['REDIRECT_MESSAGE'] .= '<br />' . $message;
        else
            $_SESSION['REDIRECT_MESSAGE'] = $message;
        exit;
    }
    public static function setMessage($message, $replace = false) {
        if($replace)
            unset($_SESSION['REDIRECT_MESSAGE']);
        
        if(is_array($message)) {
            foreach($message as $row)
                self::setMessage($row);
            return;
        }
        
        if(isset($_SESSION['REDIRECT_MESSAGE']))
            $_SESSION['REDIRECT_MESSAGE'] .= '<br />' . $message;
        else
            $_SESSION['REDIRECT_MESSAGE'] = $message;
    }
    public static function getMessage() {
        if(isset($_SESSION['REDIRECT_MESSAGE'])) {
            $message = $_SESSION['REDIRECT_MESSAGE'];
            unset($_SESSION['REDIRECT_MESSAGE']);
        } else {
            $message = '';
        }
        return $message;
    }
}