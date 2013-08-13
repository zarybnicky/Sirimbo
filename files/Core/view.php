<?php
class View {
	public $__layout;
	
	
	public static function redirect($link, $message = '', $replaceMessage = false) {
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
		if(isset($_SESSION['REDIRECT_MESSAGE']) && $replace == false)
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
?>
