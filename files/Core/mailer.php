<?php
class Mailer {
	const DEFAULT_FROM_MAIL = "TK Olymp.cz <noreply@tkolymp.cz>";
	
	private static function _mail($to, $subject, $message, $from, $headers) {
		if(empty($from) || !$from)
			$from = $this::DEFAULT_FROM_MAIL;
		$headers = "From: $from\r\n" . ($headers ? $headers . "\r\n" : '');
		$headers .= "MIME-Version: 1.0\r\n";
		$headers .= "Content-type: text/plain; charset=utf-8\r\n";
		$headers .=	"Content-Transfer-Encoding: 8bit";
		
		mail($to, "=?utf-8?B?" . base64_encode($subject) . "?=", $message, $headers);
	}
	
	public static function custom_mail($to, $subject, $message, $from = '', $headers = '') {
		Mailer::_mail($to, $subject, $message, $from, $headers);
	}
	
	public static function new_password($to, $newpass) {
		$subject = "TKOlymp.cz - nové heslo";
		$message = <<<EOS
Vy nebo někdo jiný jste požádali jste o vygenerování nového hesla.
Heslo si můžete změnit hned po přihlášení v nabídce Profil.

Heslo:
$newpass

S pozdravem
TKOlymp.cz
EOS;
		Mailer::_mail($to, $subject, $message, $this::DEFAULT_FROM_MAIL, "");
	}
	
	public static function new_user_notice($to, $username, $total_users) {
		$subject = "TKOlymp.cz - nový uživatel ($username)";
		$message = "Na TKOlymp.cz se registroval uživatel $username a čeká na potvrzení registrace.\n";
		if($total_users > 0)
			$message .= "Celkem nepotvrzených uživatelů: $total_users";
		Mailer::_mail($to, $subject, $message, $this::DEFAULT_FROM_MAIL, "");
	}
}
?>