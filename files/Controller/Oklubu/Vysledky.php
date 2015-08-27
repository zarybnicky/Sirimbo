<?php
require_once 'files/Controller/Oklubu.php';
class Controller_Oklubu_Vysledky extends Controller_Oklubu
{
    public function view($request)
    {
        $this->render(
            'files/View/Main/OKlubu/VCislech.inc',
            array(
                'sidebar' => $this->sidebar($request->getUri())
            )
        );
    }

    public function mistrovstvi($request)
    {
        $this->render(
            'files/View/Main/OKlubu/Mistrovstvi.inc',
            array(
                'sidebar' => $this->sidebar($request->getUri())
            )
        );
    }

    public function druzstva($request)
    {
        $this->render(
            'files/View/Main/OKlubu/Druzstva.inc',
            array(
                'sidebar' => $this->sidebar($request->getUri())
            )
        );
    }

    public function liga($request)
    {
        $this->render(
            'files/View/Main/OKlubu/Liga.inc',
            array(
                'sidebar' => $this->sidebar($request->getUri())
            )
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
