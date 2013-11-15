<?php
namespace TKOlomouc\Controller;

use TKOlomouc\Utility\Request;

class Home extends ControllerAbstract
{
    function view($id = null) {
        if (NABOR && Request::getURL() == '/') {
            $this->redirect('/nabor');
        }
        if (Request::getURL() == '/') {
            $this->redirect('/home');
        }

        $this->render('src/application/View/Main/Home.inc');
    }
    function sidebar() { }
}
?>