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

        $data = DBUser::getUserByNameEmail(
            strtolower($request->post('name')),
            $request->post('email')
        );

        if (!$data) {
            $this->redirect()->warning('Špatná kombinace uživatelského jména a emailu.');
            $this->redirect('/nopassword');
        }

        $base = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';
        $password = substr(str_shuffle(str_repeat($base, 5)), 0, 8);
        $passwordCrypt = User::crypt($password);

        DBUser::setPassword($data->getId(), $passwordCrypt);

        if (!DBUser::checkUser($data->getLogin(), $passwordCrypt)) {
            $this->redirect()->danger('Nepodařilo se změnit heslo, prosím kontaktujte administrátora.');
            $this->redirect('/nopassword');
        }

        Mailer::newPassword($data->getEmail(), $password);

        $this->redirect()->success('Heslo bylo úspěšně změněno, za chvíli byste jej měli obdržet v e-mailu');
        $this->redirect('/');
    }
}
