<?php
require 'phpmailer.php';
require 'smtp.php';

class Mailer
{
    private static function _mail($to, $subject, $message, $from, $headers)
    {
        $mail = new PHPMailer();

        $mail->isSMTP();
        $mail->Host = 'mail.zarybnicky.com';
        $mail->SMTPAuth = true;
        $mail->Username = 'jakub@zarybnicky.com';
        $mail->Password = '***REMOVED***';
        $mail->SMTPSecure = 'tls';
        $mail->Port = 587;
        $mail->CharSet = 'utf-8';

        $mail->setFrom('noreply@tkolymp.cz', 'TK Olymp.cz');
        foreach (is_array($to) ? $to : array($to) as $addr) {
            $mail->addAddress($addr, '');
        }
        $mail->Subject = $subject;
        $mail->Body = $message;

        $mail->send();
    }

    public static function customMail($to, $subject, $message, $from = '', $headers = '')
    {
        Mailer::_mail($to, $subject, $message, $from, $headers);
    }

    public static function newPassword($to, $newpass)
    {
        $subject = "TKOlymp.cz - nové heslo";
        $message = <<<EOS
Vy nebo někdo jiný jste požádali jste o vygenerování nového hesla.
Heslo si můžete změnit hned po přihlášení v nabídce Profil.

Dočasné heslo:
$newpass

S pozdravem
TKOlymp.cz
EOS;
        Mailer::_mail($to, $subject, $message, DEFAULT_FROM_MAIL, "");
    }

    public static function newUserNotice($to, $username, $total_users = -1)
    {
        if ($total_users == -1) {
            $total_users = count(DBUser::getNewUsers());
        }

        $subject = "TKOlymp.cz - nový uživatel ($username)";
        $message = "Na TKOlymp.cz se registroval uživatel $username a čeká na potvrzení registrace.\n";
        if ($total_users > 0)
            $message .= "Celkem nepotvrzených uživatelů: $total_users";

        Mailer::_mail($to, $subject, $message, DEFAULT_FROM_MAIL, "");
    }

    public static function registrationConfirmNotice($to, $username)
    {
        $subject = "TKOlymp.cz - potvrzení registrace";
        $message = <<<EOS
Vaše registrace (uživatel '$username') na webu TKOlymp.cz byla potvrzena.
Nyní se už můžete přihlásit s údaji, které jste zadali při registraci.

S pozdravem
TKOlymp.cz
EOS;
        Mailer::_mail($to, $subject, $message, DEFAULT_FROM_MAIL, "");
    }
}
