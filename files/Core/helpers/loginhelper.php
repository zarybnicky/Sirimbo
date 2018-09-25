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
            $name = User::getUserWholeName();
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
