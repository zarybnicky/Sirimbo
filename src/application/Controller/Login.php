<?php
namespace TKOlomouc\Controller;

use TKOlomouc\Utility\User;

class Login extends ControllerAbstract
{
    public function view($id = null)
    {
        if (User::isLogged()) {
            if (get('return')) {
                $this->redirect(get('return'));
            } else {
                $this->redirect('/member/home');
            }
        }
        $this->render('src/application/View/Main/Login.inc');
    }

    public function sidebar()
    {
        ;
    }
}
