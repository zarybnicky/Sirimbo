<?php
class Controller_Login extends Controller_Abstract
{
    public function view($request) {
        if (User::isLogged()) {
            $uri = $request->get('return') ? $request->get('return') : '/member/home';
            $this->redirect($uri);
        }
        $this->render('files/View/Main/Login.inc');
    }
}
