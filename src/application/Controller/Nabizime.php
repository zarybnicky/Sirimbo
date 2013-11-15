<?php
namespace TKOlomouc\Controller;

use TKOlomouc\Utility\Sidebar;

class Nabizime extends ControllerAbstract
{
    function view($id = null) {
        $this->redirect('/nabizime/obecne');
    }
    function obecne($id = null) {
        $this->render('src/application/View/Main/Nabizime/Main.inc');
    }
    function vystoupeni($id = null) {
        $this->render('src/application/View/Main/Nabizime/Vystoupeni.inc');
    }
    function individualky($id = null) {
        $this->render('src/application/View/Main/Nabizime/Individualky.inc');
    }
    function seminare($id = null) {
        $this->render('src/application/View/Main/Nabizime/Seminare.inc');
    }
    function soustredeni($id = null) {
        $this->render('src/application/View/Main/Nabizime/Seminare.inc');
    }
    function sidebar() {
        $s = new Sidebar();

        echo $s->menuHeader();
        echo $s->menuItem('Nabízíme',            '/nabizime/obecne');
        echo $s->menuItem('Taneční vystoupení',  '/nabizime/vystoupeni');
        echo $s->menuItem('Individuální lekce',  '/nabizime/individualky');
        echo $s->menuItem('Skupinové semináře',  '/nabizime/seminare');
        echo $s->menuItem('Taneční soustředění', '/nabizime/soustredeni');

        echo $s->commonItems();
    }
}
?>