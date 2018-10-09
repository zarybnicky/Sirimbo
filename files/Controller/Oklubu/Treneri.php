<?php
require_once 'files/Controller/Oklubu.php';
class Controller_Oklubu_Treneri extends Controller_Oklubu
{
    public function view()
    {
        $this->redirect('/oklubu/klubovi');
    }

    public function klubovi()
    {
        $this->render('files/View/Main/OKlubu/TreneriInt.inc', [
            'header' => 'Kluboví trenéři'
        ]);
    }

    public function externi()
    {
        $this->render('files/View/Main/OKlubu/TreneriExt.inc', [
            'header' => 'Externí trenéři'
        ]);
    }
}
