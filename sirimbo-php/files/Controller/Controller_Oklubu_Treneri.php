<?php
class Controller_Oklubu_Treneri extends Controller_Abstract
{
    public function view($request)
    {
        $this->redirect('/oklubu/klubovi');
    }

    public function klubovi($request)
    {
        $this->render('files/View/Main/OKlubu/TreneriInt.inc', [
            'header' => 'Kluboví trenéři'
        ]);
    }

    public function externi($request)
    {
        $this->render('files/View/Main/OKlubu/TreneriExt.inc', [
            'header' => 'Externí trenéři'
        ]);
    }
}