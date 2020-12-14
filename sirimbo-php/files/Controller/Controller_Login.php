<?php
class Controller_Login extends Controller_Abstract
{
    public function view($request)
    {
        if (Session::isLogged()) {
            $uri = $request->get('return') ? $request->get('return') : '/member/home';
            new \RedirectHelper($uri);
        }
        new \RenderHelper('files/View/Main/Login.inc');
    }
}
