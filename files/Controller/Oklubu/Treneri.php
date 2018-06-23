<?php
require_once 'files/Controller/Oklubu.php';
class Controller_Oklubu_Treneri extends Controller_Oklubu
{
    public function view($request)
    {
        $this->redirect('/oklubu/klubovi');
    }

    public function klubovi($request)
    {
        $this->render('files/View/Main/OKlubu/TreneriInt.inc');
    }

    public function externi($request)
    {
        $this->render('files/View/Main/OKlubu/TreneriExt.inc');
    }
}
