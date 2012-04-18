<?php
class Mailer {
	const DEFAULT_FROM_MAIL = "TK Olymp.cz <noreply@tkolymp.cz>";
	
	private static function _mail($to, $subject, $message, $from, $headers = '') {
		if(empty($from) || !$from)
			$from = $this::DEFAULT_FROM_MAIL;
		$headers .= "From: $from\r\n";
		$headers .= "MIME-Version: 1.0\r\n";
		$headers .= "Content-type: text/plain; charset=utf-8\r\n";
		$headers .=	"Content-Transfer-Encoding: 8bit";
		
		mail($to, "=?utf-8?B?" . base64_encode($subject) . "?=", $message, $headers);
	}
	
	public static function custom_mail($to, $subject, $message, $from = '', $headers = '') {
		$this::_mail($to, $subject, $message, $from, $headers);
	}
}
?>