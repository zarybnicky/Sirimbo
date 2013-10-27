<?php
class COntroller_Oklubu extends Controller_Abstract {
    function view($id = null) {
       $this->redirect('/oklubu/obecne');
    }
    function obecne($id = null) {
        $this->render('files/View/Main/OKlubu/Main.inc');
    }
    function historie($id = null) {
        $this->render('files/View/Main/OKlubu/Historie.inc');
    }
    function uspechy($id = null) {
        $this->render('files/View/Main/OKlubu/VCislech.inc');
    }
    function mistrovstvi($id = null) {
        $this->render('files/View/Main/OKlubu/Mistrovstvi.inc');
    }
    function druzstva($id = null) {
        $this->render('files/View/Main/OKlubu/Druzstva.inc');
    }
    function liga($id = null) {
        $this->render('files/View/Main/OKlubu/Liga.inc');
    }
    function klubovi($id = null) {
        $this->render('files/View/Main/OKlubu/TreneriInt.inc');
    }
    function externi($id = null) {
        $this->render('files/View/Main/OKlubu/TreneriExt.inc');
    }
    function saly($id = null) {
        $this->render('files/View/Main/OKlubu/Saly.inc');
    }
    function sidebar() {
        $s = new Sidebar();
        
        echo $s->menuHeader();
        echo $s->menuItem('Historie',            '/oklubu/historie');
        echo $s->menuItem('Úspěchy v číslech',    '/oklubu/uspechy');
        echo $s->menuItem('Mistrovství ČR',        '/oklubu/mistrovstvi');
        echo $s->menuItem('Družstva',            '/oklubu/druzstva');
        echo $s->menuItem('Taneční liga',        '/oklubu/liga');
        echo $s->menuItem('Kluboví trenéři',    '/oklubu/treneri/klubovi');
        echo $s->menuItem('Externí trenéři',    '/oklubu/treneri/externi');
        echo $s->menuItem('Kde trénujeme',        '/oklubu/saly');
        echo $s->menuItem('Stanovy klubu',        '/oklubu/stanovy.pdf');
        
        echo $s->commonItems();
    }
}
?>