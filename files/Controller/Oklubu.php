<?php
class Controller_Oklubu extends Controller_Abstract
{
    public function view($id = null) {
       $this->redirect('/oklubu/obecne');
    }
    public function obecne($id = null) {
        $this->render('files/View/Main/OKlubu/Main.inc');
    }
    public function historie($id = null) {
        $this->render('files/View/Main/OKlubu/Historie.inc');
    }
    public function uspechy($id = null) {
        $this->render('files/View/Main/OKlubu/VCislech.inc');
    }
    public function mistrovstvi($id = null) {
        $this->render('files/View/Main/OKlubu/Mistrovstvi.inc');
    }
    public function druzstva($id = null) {
        $this->render('files/View/Main/OKlubu/Druzstva.inc');
    }
    public function liga($id = null) {
        $this->render('files/View/Main/OKlubu/Liga.inc');
    }
    public function klubovi($id = null) {
        $this->render('files/View/Main/OKlubu/TreneriInt.inc');
    }
    public function externi($id = null) {
        $this->render('files/View/Main/OKlubu/TreneriExt.inc');
    }
    public function saly($id = null) {
        $this->render('files/View/Main/OKlubu/Saly.inc');
    }
    public function navbar() {
        return parent::navbar() .
            new Navbar(
                include SETTINGS . '/menu/oklubu.php',
                false
            );
    }
}
