<?php
class Controller_Error extends Controller_Abstract {
	function view($id = null) {
		if(!get('id'))
			$notice = 'Tato stránka není určena k přímému prohlížení. Prosím, přejděte <a href="/">sem</a>';
		else if(!isset(Settings::$errors[get('id')]) || !file_exists(Settings::$errors[get('id')]))
			$notice = "Chybová stránka s daným ID nebyla nalezena";
		else {
			ob_start();
			include(Settings::$errors[get('id')]);
			$notice = ob_get_contents();
			ob_end_clean();
		}
		
		$this->render('files/View/Empty.inc', array(
				'nadpis' => 'Chyba',
				'notice' => $notice
		));
	}
	function sidebar() { }
}
?>