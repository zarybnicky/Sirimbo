<?php
class LoginHelper
{
    public function __toString()
    {
        if ($user = \Session::getUser()) {
            $name = $user->getFullName();
            return "<li class=\"userbox nav-item nav-link\"><a href=\"/member/profil\"><i class=\"fas fa-user\"></i> $name</a></li>";
        } else {
            $r = new Renderer();
            return $r->render('files/View/Login.inc');
        }
    }
}
