<?php
class Controller_Logout extends Controller_Abstract
{
    public function view($request) {
        User::logout();
        $this->redirect('/home', 'Byli jste úspěšně odhlášeni.');
    }
}
