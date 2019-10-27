<?php
class Controller_Logout extends Controller_Abstract
{
    public function view($request)
    {
        Session::logout();
        $this->redirect('/');
    }
}
