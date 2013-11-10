<?php
class Controller_Login extends Controller_Abstract
{
    function view($id = null) {
        if (User::isLogged()) {
            if (get('return'))
                $this->redirect(get('return'));
            else
                $this->redirect('/member/home');
        }
        $this->render('src/application/View/Main/Login.inc');
    }
    function sidebar() { }
}
?>