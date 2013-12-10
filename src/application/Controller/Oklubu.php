<?php
namespace TKOlomouc\Controller;

use TKOlomouc\Utility\Sidebar;

class Oklubu extends ControllerAbstract
{
    public function view($id = null)
    {
        $this->redirect('/oklubu/obecne');
    }

    public function obecne($id = null)
    {
        $this->render('src/application/View/Main/OKlubu/Main.inc');
    }

    public function historie($id = null)
    {
        $this->render('src/application/View/Main/OKlubu/Historie.inc');
    }

    public function uspechy($id = null)
    {
        $this->render('src/application/View/Main/OKlubu/VCislech.inc');
    }

    public function mistrovstvi($id = null)
    {
        $this->render('src/application/View/Main/OKlubu/Mistrovstvi.inc');
    }

    public function druzstva($id = null)
    {
        $this->render('src/application/View/Main/OKlubu/Druzstva.inc');
    }

    public function liga($id = null)
    {
        $this->render('src/application/View/Main/OKlubu/Liga.inc');
    }

    public function klubovi($id = null)
    {
        $this->render('src/application/View/Main/OKlubu/TreneriInt.inc');
    }

    public function externi($id = null)
    {
        $this->render('src/application/View/Main/OKlubu/TreneriExt.inc');
    }

    public function saly($id = null)
    {
        $this->render('src/application/View/Main/OKlubu/Saly.inc');
    }

    public function sidebar()
    {
        $s = new Sidebar();

        echo $s->menuHeader();
        echo $s->menuItem('Historie', '/oklubu/historie');
        echo $s->menuItem('Úspěchy v číslech', '/oklubu/uspechy');
        echo $s->menuItem('Mistrovství ČR', '/oklubu/mistrovstvi');
        echo $s->menuItem('Družstva', '/oklubu/druzstva');
        echo $s->menuItem('Taneční liga', '/oklubu/liga');
        echo $s->menuItem('Kluboví trenéři', '/oklubu/treneri/klubovi');
        echo $s->menuItem('Externí trenéři', '/oklubu/treneri/externi');
        echo $s->menuItem('Kde trénujeme', '/oklubu/saly');
        echo $s->menuItem('Stanovy klubu', '/oklubu/stanovy.pdf');

        echo $s->commonItems();
    }
}
