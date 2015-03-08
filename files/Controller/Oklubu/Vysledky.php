<?php
require_once 'files/Controller/Oklubu.php';
class Controller_Oklubu_Vysledky extends Controller_Oklubu
{
    public function view($request)
    {
        $this->request = $request;
        $this->render('files/View/Main/OKlubu/VCislech.inc');
    }
    
    public function mistrovstvi($request)
    {
        $this->request = $request;
        $this->render('files/View/Main/OKlubu/Mistrovstvi.inc');
    }
    
    public function druzstva($request)
    {
        $this->request = $request;
        $this->render('files/View/Main/OKlubu/Druzstva.inc');
    }
    
    public function liga($request)
    {
        $this->request = $request;
        $this->render('files/View/Main/OKlubu/Liga.inc');
    }

    public function sidebar()
    {
        return new Sidebar(
            $this->request,
            include SETTINGS . '/menu/oklubu.vysledky.php'
        );
    }
}
