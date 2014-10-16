<?php
class Controller_Logout extends Controller_Abstract
{
    public function view($id = null) {
        User::logout();
        $this->redirect('/home', 'Byli jste úspěšně odhlášeni.');
    }
}
