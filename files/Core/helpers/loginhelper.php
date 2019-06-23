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
            $template = 'files/View/Helper/Userbox.inc';
            $user = User::getUserData();
            $name = $user['u_jmeno'] . ' ' . $user['u_prijmeni'];
        } else {
            $template = 'files/View/Helper/Login.inc';
            $name = '';
        }

        $r = new Renderer();
        return $r->render($template, ['name' => $name]);
    }

    public function __toString()
    {
        return $this->render();
    }
}
