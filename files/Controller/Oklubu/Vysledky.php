<?php
require_once 'files/Controller/Oklubu.php';
class Controller_Oklubu_Vysledky extends Controller_Oklubu
{
    public function view($request)
    {
        $this->render(
            'files/View/Main/OKlubu/VCislech.inc',
            ['sidebar' => $this->sidebar($request->getUri())]
        );
    }

    public function sidebar($uri)
    {
        return new Sidebar(
            include SETTINGS . '/menu/oklubu.vysledky.php',
            $uri
        );
    }
}
