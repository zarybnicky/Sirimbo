<?php
class Controller_Error extends Controller_Abstract {
	function view($id = null) {
		header_main('Chyba');
        if(!get('id'))
        	notice('Tato stránka není určena k přímému prohlížení. Prosím, přejděte <a href="/">sem</a>');
        else if(!isset(Settings::$errors[get('id')]) || !file_exists(Settings::$errors[get('id')]))
        	notice("Chybová stránka s daným ID nebyla nalezena");
        else
        	include(Settings::$errors[get('id')]);
	}
	function sidebar() {
		
	}
}
?>