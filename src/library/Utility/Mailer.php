<?php
namespace TKOlomouc\Utility;

use TKOlomouc\Model\DBUser;

class Mailer
{
    private static function mail($to, $subject, $message, $from, $headers)
    {
        if (empty($from) || !$from) {
            $from = DEFAULT_FROM_MAIL;
        }
        $headers = (strlen($headers) > 0 ? $headers . "\n" : '')
            . "From: $from\n"
            . "MIME-Version: 1.0\n"
            . "Content-type: text/plain; charset=\"UTF-8\";\n"
            . "Content-Transfer-Encoding: 8bit\n"
            . "\n";

        if (is_array($to)) {
            foreach ($to as $email) {
                mail($email, "=?utf-8?B?" . base64_encode($subject) . "?=", $message, $headers);
            }
        } else {
            mail($to, "=?utf-8?B?" . base64_encode($subject) . "?=", $message, $headers);
        }
    }

    public static function customMail($to, $subject, $message, $from = '', $headers = '')
    {
        self::mail($to, $subject, $message, $from, $headers);
    }

    public static function newPassword($to, $newpass)
    {
        $subject = "TKOlymp.cz - nové heslo";
        $message = <<<EOS
Vy nebo někdo jiný jste požádali jste o vygenerování nového hesla.
Heslo si můžete změnit hned po přihlášení v nabídce Profil.

Heslo:
$newpass

S pozdravem
TKOlymp.cz
EOS;
        self::mail($to, $subject, $message, DEFAULT_FROM_MAIL, "");
    }

    public static function newUserUotice($to, $username, $total_users = -1)
    {
        if ($total_users == -1) {
            $total_users = count(DBUser::getNewUsers());
        }

        $subject = "TKOlymp.cz - nový uživatel ($username)";
        $message = "Na TKOlymp.cz se registroval uživatel $username a čeká na potvrzení registrace.\n";
        if ($total_users > 0) {
            $message .= "Celkem nepotvrzených uživatelů: $total_users";
        }
        self::mail($to, $subject, $message, DEFAULT_FROM_MAIL, "");
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
        self::mail($to, $subject, $message, DEFAULT_FROM_MAIL, "");
    }
}
