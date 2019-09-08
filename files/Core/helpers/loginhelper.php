<?php
class LoginHelper
{
    public function login()
    {
        return $this;
    }

    public function render()
    {
        if (User::isLogged()) {
            $user = User::getUserData();
            $name = $user['u_jmeno'] . ' ' . $user['u_prijmeni'];
            return "<li class=\"userbox nav-item nav-link\"><a href=\"/member/profil\"><i class=\"fas fa-user\"></i> $name</a></li>";
        } else {
            $template = 'files/View/Login.inc';
            $r = new Renderer();
            return $r->render($template);
        }
    }

    public function __toString()
    {
        return $this->render();
    }
}
