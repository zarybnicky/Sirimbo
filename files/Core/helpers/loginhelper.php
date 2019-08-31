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
            return "<div id=\"userbox\"><i class=\"fas fa-user\"></i><a href=\"/member/profil\">$name</a></div>";
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
