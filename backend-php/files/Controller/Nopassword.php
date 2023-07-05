<?php
namespace Olymp\Controller;

class Nopassword
{
    public static function get()
    {
        \Render::twig('Main/NoPassword.twig');
    }

    public static function post()
    {
        try {
            \DBUser::setPassword($_POST['name'] ?? '', $_POST['email'] ?? '');
            \Message::success('Heslo bylo úspěšně změněno, za chvíli byste jej měli obdržet v e-mailu');
            \Redirect::to('/');
        } catch (\Exception $e) {
            \Message::warning('Špatná kombinace uživatelského jména a emailu.');
            \Redirect::to('/nopassword');
        }

    }
}
