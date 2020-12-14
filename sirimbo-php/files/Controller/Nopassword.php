<?php
namespace Olymp\Controller;

class Nopassword
{
    public static function get()
    {
        return new \RenderHelper('files/View/Main/Nopassword.inc', [
            'header' => 'Zapomenuté heslo'
        ]);
    }

    public static function post()
    {
        $data = \DBUser::getUserByNameEmail(strtolower($_POST['name'] ?? ''), $_POST['email'] ?? '');

        if (!$data) {
            new \MessageHelper('warning', 'Špatná kombinace uživatelského jména a emailu.');
            return new \RedirectHelper('/nopassword');
        }

        $base = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';
        $password = substr(str_shuffle(str_repeat($base, 5)), 0, 8);
        $passwordCrypt = \User::crypt($password);

        \DBUser::setPassword($data->getId(), $passwordCrypt);

        if (!\DBUser::checkUser($data->getLogin(), $passwordCrypt)) {
            new \MessageHelper('danger', 'Nepodařilo se změnit heslo, prosím kontaktujte administrátora.');
            new \RedirectHelper('/nopassword');
        }

        \Mailer::newPassword($data->getEmail(), $password);

        new \MessageHelper('success', 'Heslo bylo úspěšně změněno, za chvíli byste jej měli obdržet v e-mailu');
        new \RedirectHelper('/');
    }
}
