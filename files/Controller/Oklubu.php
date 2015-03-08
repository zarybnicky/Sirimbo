<?php
class Controller_Oklubu extends Controller_Abstract
{
    public function view($request)
    {
        $this->redirect('/oklubu/obecne');
    }

    public function obecne($request)
    {
        $this->render('files/View/Main/OKlubu/Main.inc');
    }

    public function historie($request)
    {
        $this->render('files/View/Main/OKlubu/Historie.inc');
    }

    public function klubovi($request)
    {
        $this->render('files/View/Main/OKlubu/TreneriInt.inc');
    }

    public function externi($request)
    {
        $this->render('files/View/Main/OKlubu/TreneriExt.inc');
    }

    public function saly($request)
    {
        $this->render('files/View/Main/OKlubu/Saly.inc');
    }

    public function navbar()
    {
        return parent::navbar() .
            new Navbar(
                include SETTINGS . '/menu/oklubu.php',
                false
            );
    }

    public function sidebar()
    {
        return new Sidebar(
            include SETTINGS . '/menu/oklubu.php'
        );
    }
}
