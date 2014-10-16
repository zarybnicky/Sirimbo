<?php
class Controller_Login extends Controller_Abstract
{
    public function view($id = null) {
        if (User::isLogged()) {
            $uri = get('return') ? get('return') : '/member/home';
            $this->redirect($uri);
        }
        $this->render('files/View/Main/Login.inc');
    }
}
