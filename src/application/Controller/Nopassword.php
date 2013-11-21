<?php
namespace TKOlomouc\Controller;

use TKOlomouc\Utility\User;
use TKOlomouc\Utility\Mailer;
use TKOlomouc\Model\DBUser;
use TKOlomouc\Utility\Response;

class Nopassword extends ControllerAbstract
{
    public function view($id = null)
    {
        if (post('action') != 'gen_pass' && post('action') != 'enter') {
            $this->render('src/application/View/Main/Nopassword.inc');
            return;
        }

        post('name', strtolower(post('name')));

        if (!($data = DBUser::getUserDataByNameEmail(post('name'), post('email')))) {
            Response::setMessage(
                'Špatná kombinace přihlašovacího jména a emailu, zkuste to prosím znovu.'
            );
            $this->render('src/application/View/Main/Nopassword.inc');
            return;
        }

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
    }

    public function sidebar()
    {
        ;
    }
}
