<?php
namespace TKOlomouc\Controller;

use TKOlomouc\Utility\User;

class Logout extends ControllerAbstract
{
    public function view($id = null)
    {
        User::logout();
        $this->redirect('/home', 'Byli jste úspěšně odhlášeni.');
    }

    public function sidebar()
    {
        ;
    }
}
