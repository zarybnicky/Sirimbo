<?php
class Controller_Nabizime extends Controller_Abstract
{
    public function view($id = null) {
        $this->redirect('/nabizime/obecne');
    }
    public function obecne($id = null) {
        $this->render('files/View/Main/Nabizime/Main.inc');
    }
    public function vystoupeni($id = null) {
        $this->render('files/View/Main/Nabizime/Vystoupeni.inc');
    }
    public function individualky($id = null) {
        $this->render('files/View/Main/Nabizime/Individualky.inc');
    }
    public function seminare($id = null) {
        $this->render('files/View/Main/Nabizime/Seminare.inc');
    }
    public function soustredeni($id = null) {
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
