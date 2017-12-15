<?php
require_once 'files/Controller/Oklubu/Vysledky.php';
class Controller_Oklubu_Vysledky_Mistrovstvi extends Controller_Oklubu_Vysledky
{
    public function get($request)
    {
        $this->render(
            'files/View/Main/OKlubu/Mistrovstvi.inc',
            ['sidebar' => $this->sidebar($request->getUri())]
        );
    }
}
