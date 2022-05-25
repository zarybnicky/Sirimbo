<?php
class Mailer
{
    private static function _mail($to, $subject, $message)
    {
        $mail = new PHPMailer();

        $mail->isSMTP();
        $mail->Host = SMTP_HOST;
        $mail->SMTPAuth = SMTP_AUTH;
        if (SMTP_AUTH) {
            $mail->Username = SMTP_USER;
            $mail->Password = SMTP_PASS;
        }
        $mail->SMTPSecure = SMTP_TLS ? 'tls' : '';
        $mail->Port = SMTP_PORT;
        $mail->CharSet = 'utf-8';

        $mail->setFrom(DEFAULT_FROM_MAIL, 'TK Olymp');
        foreach (is_array($to) ? $to : [$to] as $addr) {
            $mail->addAddress($addr, '');
        }
        $mail->Subject = $subject;
        $mail->Body = $message;
        $mail->send();
    }

    public static function newPassword($to, $newpass)
    {
        $subject = "[Olymp] Nové heslo";
        $message = <<<EOS
Vy nebo někdo jiný jste požádali jste o vygenerování nového hesla.
Heslo si můžete změnit hned po přihlášení v nabídce Profil.

Dočasné heslo:
$newpass

S pozdravem
TKOlymp.cz
EOS;
        Mailer::_mail($to, $subject, $message);
    }

    public static function newUserNotice($to, $username, $total_users = -1)
    {
        if ($total_users == -1) {
            $total_users = count(\DBUser::getNewUsers());
        }

        $subject = "[Olymp] Nová registrace ($username)";
        $message = "Na TKOlymp.cz se registroval uživatel $username a čeká na potvrzení registrace.\n";
        if ($total_users > 0) {
            $message .= "Celkem nepotvrzených uživatelů: $total_users";
        }
        Mailer::_mail($to, $subject, $message);
    }

    public static function registrationConfirmNotice($to, $username)
    {
        $subject = "[Olymp] Potvrzení registrace";
        $message = <<<EOS
Vaše registrace (uživatel '$username') na webu TKOlymp.cz byla potvrzena.
Nyní se už můžete přihlásit s údaji, které jste zadali při registraci.

S pozdravem
TKOlymp.cz
EOS;
        Mailer::_mail($to, $subject, $message);
    }
}
