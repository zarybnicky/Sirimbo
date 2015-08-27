<?php
class Controller_Nabizime extends Controller_Abstract
{
    public function view($request) {
        $this->redirect('/nabizime/obecne');
    }

    public function obecne($request) {
        $this->render('files/View/Main/Nabizime/Main.inc');
    }

    public function vystoupeni($request) {
        $this->render('files/View/Main/Nabizime/Vystoupeni.inc');
    }

    public function individualky($request) {
        $this->render('files/View/Main/Nabizime/Individualky.inc');
    }

    public function seminare($request) {
        $this->render('files/View/Main/Nabizime/Seminare.inc');
    }

    public function soustredeni($request) {
        $this->render('files/View/Main/Nabizime/Seminare.inc');
    }

    public function navbar() {
        return parent::navbar() .
            new Navbar(
                include SETTINGS . '/menu/nabizime.php',
                false
            );
    }
}
