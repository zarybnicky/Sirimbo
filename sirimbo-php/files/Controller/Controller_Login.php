<?php
class Controller_Login
{
    public function view($request)
    {
        if (\Session::isLogged()) {
            $uri = $_GET['return'] ? $_GET['return'] : '/member';
            new \RedirectHelper($uri);
        }
        new \RenderHelper('files/View/Main/Login.inc');
    }
}
