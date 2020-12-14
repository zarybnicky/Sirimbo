<?php
class Controller_Login
{
    public function view($request)
    {
        if (Session::isLogged()) {
            $uri = $request->get('return') ? $request->get('return') : '/member';
            new \RedirectHelper($uri);
        }
        new \RenderHelper('files/View/Main/Login.inc');
    }
}
