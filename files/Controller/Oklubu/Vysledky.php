<?php
require_once 'files/Controller/Oklubu.php';
class Controller_Oklubu_Vysledky extends Controller_Oklubu
{
    public function view($request)
    {
        $this->render('files/View/Main/OKlubu/VCislech.inc', [
            'header' => 'Úspěchy v číslech'
        ]);
    }

    public function druzstva($request)
    {
        $this->render('files/View/Main/OKlubu/Druzstva.inc', [
            'header' => 'Výsledky našich párů na Mistrovství České republiky družstev'
        ]);
    }

    public function liga($request)
    {
        $this->render('files/View/Main/OKlubu/Liga.inc', [
            'header' => 'Výsledky našich párů na taneční lize'
        ]);
    }

    public function mistrovstvi($request)
    {
        $this->render('files/View/Main/OKlubu/Mistrovstvi.inc', [
            'header' => 'Výsledky našich párů na Mistrovství ČR'
        ]);
    }
}
