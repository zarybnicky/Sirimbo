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
    function sidebar() {
        $s = new Sidebar();

        echo $s->menuItem('Nabízíme',            '/nabizime/obecne');
        echo $s->menuItem('Taneční vystoupení',  '/nabizime/vystoupeni');
        echo $s->menuItem('Individuální lekce',  '/nabizime/individualky');
        echo $s->menuItem('Skupinové semináře',  '/nabizime/seminare');
        echo $s->menuItem('Taneční soustředění', '/nabizime/soustredeni');
    }
}
