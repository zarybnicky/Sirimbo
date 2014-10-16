<?php
class Controller_Nabizime extends Controller_Abstract
{
    function view($id = null) {
        $this->redirect('/nabizime/obecne');
    }
    function obecne($id = null) {
        $this->render('files/View/Main/Nabizime/Main.inc');
    }
    function vystoupeni($id = null) {
        $this->render('files/View/Main/Nabizime/Vystoupeni.inc');
    }
    function individualky($id = null) {
        $this->render('files/View/Main/Nabizime/Individualky.inc');
    }
    function seminare($id = null) {
        $this->render('files/View/Main/Nabizime/Seminare.inc');
    }
    function soustredeni($id = null) {
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
