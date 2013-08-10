<?php
class Controller_Nabizime extends Controller_Abstract {
    function view($id = null) {
        View::redirect('/nabizime/obecne');
    }
    function sidebar() {
		$s = new Sidebar();
		
		echo $s->menuHeader();
		echo $s->menuItem('Nabízíme',			'/nabizime/obecne');
		echo $s->menuItem('Taneční vystoupení',	'/nabizime/vystoupeni');
		echo $s->menuItem('Individuální lekce',	'/nabizime/indivualky');
		echo $s->menuItem('Skupinové semináře',	'/nabizime/seminare');
		echo $s->menuItem('Taneční soustředění','/nabizime/soustredeni');
		
		echo $s->commonItems();
    }
    function obecne($id = null) {
        include('files/Main/Nabizime/Main.inc');
    }
    function vystoupeni($id = null) {
        include('files/Main/Nabizime/Vystoupeni.inc');
    }
    function individualky($id = null) {
        include('files/Main/Nabizime/Individualky.inc');
    }
    function seminare($id = null) {
        include('files/Main/Nabizime/Seminare.inc');
    }
    function soustredeni($id = null) {
        include('files/Main/Nabizime/Seminare.inc');
    }
}
?>