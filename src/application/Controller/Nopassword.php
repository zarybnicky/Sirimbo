<?php
class Controller_Nopassword extends Controller_Abstract
{
    function view($id = null) {
        if (post('action') == 'gen_pass' || post('action') == 'enter') {
            post('name', strtolower(post('name')));

            if ($data = DBUser::getUserDataByNameEmail(post('name'), post('email'))) {
                $base = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';
                $password = substr(str_shuffle(str_repeat($base, 5)), 0, 8);
                $passwordCrypt = User::crypt($password);

                DBUser::setPassword($data['u_id'], $passwordCrypt);

                if (DBUser::checkUser($data['u_login'], $passwordCrypt)) {
                    Mailer::newPassword($data['u_email'], $password);
                    $this->redirect(
                        '/home',
                        'Povedlo se, za chvíli byste měli dostat e-mail s novým heslem.<br/>'
                        . 'V případě problémů prosím kontaktujte administrátora.'
                    );
                } else {
                    $this->redirect()->setMessage(
                        'Něco se nepovedlo, prosím kontaktujte administrátory.'
                    );
                }
            } else {
                $this->redirect()->setMessage(
                    'Špatná kombinace přihlašovacího jména a emailu, zkuste to prosím znovu.'
                );
            }
        }
        $this->render('src/application/View/Main/Nopassword.inc');
    }
    function sidebar() { }
}
?>