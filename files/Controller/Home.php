<?php
class Controller_Home extends Controller_Abstract
{
    function view($id = null) {
        if (NABOR && Request::getURL() == '/')
            $this->redirect('/nabor');
        if (Request::getURL() == '/')
            $this->redirect('/home');

        $this->render('files/View/Main/Home.inc');
    }
    function sidebar() { }
}
?>