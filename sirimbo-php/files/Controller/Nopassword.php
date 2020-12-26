<?php
namespace Olymp\Controller;

class Nopassword
{
    public static function get()
    {
        \Render::twig('Main/NoPassword.twig', [
            'header' => 'Zapomenuté heslo'
        ]);
    }

    public static function post()
    {
        $data = \DBUser::getUserByNameEmail(strtolower($_POST['name'] ?? ''), $_POST['email'] ?? '');

        if (!$data) {
            \Message::warning('Špatná kombinace uživatelského jména a emailu.');
            \Redirect::to('/nopassword');
        }

        $base = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';
        $password = substr(str_shuffle(str_repeat($base, 5)), 0, 8);
        $passwordCrypt = \User::crypt($password);

        \DBUser::setPassword($data->getId(), $passwordCrypt);

        if (!\DBUser::checkUser($data->getLogin(), $passwordCrypt)) {
            \Message::danger('Nepodařilo se změnit heslo, prosím kontaktujte administrátora.');
            \Redirect::to('/nopassword');
        }

        \Mailer::newPassword($data->getEmail(), $password);

        \Message::success('Heslo bylo úspěšně změněno, za chvíli byste jej měli obdržet v e-mailu');
        \Redirect::to('/');
    }
}
