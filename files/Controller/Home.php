<?php
class Controller_Home extends Controller_Abstract
{
    public function view($request) {
        if (!$request->getURI()) {
            if (NABOR) {
                $this->redirect('/nabor');
            }
            $this->redirect('/home');
        }
        $this->render('files/View/Main/Home.inc');
    }
}
