<?php
class Controller_Home extends Controller_Abstract
{
    function view($id = null) {
        if (!Request::getURI()) {
            if (NABOR) {
                $this->redirect('/nabor');
            }
            $this->redirect('/home');
        }
        $this->render('files/View/Main/Home.inc');
    }
}
