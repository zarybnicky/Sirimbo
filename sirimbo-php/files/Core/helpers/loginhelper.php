<?php
class LoginHelper
{
    public function login()
    {
        return $this;
    }

    public function render()
    {
        if (Session::isLogged()) {
            $name = Session::getUserData()->getFullName();
            return "<li class=\"userbox nav-item nav-link\"><a href=\"/member/profil\"><i class=\"fas fa-user\"></i> $name</a></li>";
        } else {
            $r = new Renderer();
            return $r->render('files/View/Login.inc');
        }
    }

    public function __toString()
    {
        return $this->render();
    }
}