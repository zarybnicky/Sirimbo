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

    public function sidebar() {
        return new Sidebar(
            include SETTINGS . '/menu/oklubu.php'
        );
    }
}
