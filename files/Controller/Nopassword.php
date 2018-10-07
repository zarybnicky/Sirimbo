<?php
class Controller_Nopassword extends Controller_Abstract
{
    public function view($request)
    {
        if (!in_array($request->post('action'), ['gen_pass', 'enter'])) {
            $this->render('files/View/Main/Nopassword.inc', [
                'header' => 'Zapomenuté heslo'
            ]);
            return;
        }

        $data = DBUser::getUserDataByNameEmail(
            strtolower($request->post('name')),
            $request->post('email')
        );

        if (!$data) {
            $this->redirect(
                '/nopassword',
                'Špatná kombinace uživatelského jména a emailu.'
            );
        }

        $base = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';
        $password = substr(str_shuffle(str_repeat($base, 5)), 0, 8);
        $passwordCrypt = User::crypt($password);

        DBUser::setPassword($data['u_id'], $passwordCrypt);

        if (!DBUser::checkUser($data['u_login'], $passwordCrypt)) {
            $this->redirect(
                '/nopassword',
                'Nepodařilo se změnit heslo, prosím kontaktujte administrátora.'
            );
        }

        Mailer::newPassword($data['u_email'], $password);

        $this->redirect(
            '/home',
            'Heslo bylo úspěšně změněno, za chvíli byste jej měli obdržet v e-mailu'
        );
    }
}
