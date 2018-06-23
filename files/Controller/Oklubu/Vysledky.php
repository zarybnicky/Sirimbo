<?php
require_once 'files/Controller/Oklubu.php';
class Controller_Oklubu_Vysledky extends Controller_Oklubu
{
    public function view($request)
    {
        $this->render('files/View/Main/OKlubu/VCislech.inc');
    }

    public function druzstva($request)
    {
        $this->render('files/View/Main/OKlubu/Druzstva.inc');
    }

    public function liga($request)
    {
        $this->render('files/View/Main/OKlubu/Liga.inc');
    }

    public function mistrovstvi($request)
    {
        $this->render('files/View/Main/OKlubu/Mistrovstvi.inc');
    }
}
