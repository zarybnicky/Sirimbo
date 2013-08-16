<?php
class RedirectHelper {
	public function redirect() {
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
	public static function setRedirectMessage($message, $replace = false) {
		if($replace)
			unset($_SESSION['REDIRECT_MESSAGE']);
		
		if(is_array($message)) {
			foreach($message as $row)
				self::setRedirectMessage($row);
			return;
		}
		
		if(isset($_SESSION['REDIRECT_MESSAGE']))
			$_SESSION['REDIRECT_MESSAGE'] .= '<br />' . $message;
		else
			$_SESSION['REDIRECT_MESSAGE'] = $message;
	}
	public static function getRedirectMessage() {
		if(isset($_SESSION['REDIRECT_MESSAGE'])) {
			$message = $_SESSION['REDIRECT_MESSAGE'];
			unset($_SESSION['REDIRECT_MESSAGE']);
		} else {
			$message = '';
		}
		return $message;
	}
}