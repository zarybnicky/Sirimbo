<?php
class Controller_Oklubu extends Controller_Abstract
{
    public function view($request)
    {
        $this->redirect('/');
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

    public function saly($request)
    {
        $this->render('files/View/Main/OKlubu/Saly.inc', [
            'header' => 'Kde trénujeme'
        ]);
    }
}