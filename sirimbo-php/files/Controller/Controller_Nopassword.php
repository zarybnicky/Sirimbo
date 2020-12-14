<?php
class Controller_Nopassword
{
    public function view($request)
    {
        if (!in_array($request->post('action'), ['gen_pass', 'enter'])) {
            return new \RenderHelper('files/View/Main/Nopassword.inc', [
                'header' => 'Zapomenuté heslo'
            ]);
        }

        $data = DBUser::getUserByNameEmail(
            strtolower($request->post('name')),
            $request->post('email')
        );

        if (!$data) {
            new \MessageHelper('warning', 'Špatná kombinace uživatelského jména a emailu.');
            return new \RedirectHelper('/nopassword');
        }

        $base = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';
        $password = substr(str_shuffle(str_repeat($base, 5)), 0, 8);
        $passwordCrypt = User::crypt($password);

        DBUser::setPassword($data->getId(), $passwordCrypt);

        if (!DBUser::checkUser($data->getLogin(), $passwordCrypt)) {
            new \MessageHelper('danger', 'Nepodařilo se změnit heslo, prosím kontaktujte administrátora.');
            new \RedirectHelper('/nopassword');
        }

        Mailer::newPassword($data->getEmail(), $password);

        new \MessageHelper('success', 'Heslo bylo úspěšně změněno, za chvíli byste jej měli obdržet v e-mailu');
        new \RedirectHelper('/');
    }
}
