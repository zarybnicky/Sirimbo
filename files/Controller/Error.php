<?php
class Controller_Error extends Controller_Abstract {
	function view($id = null) {
		if(!get('id')) get('id', '');
		
		function ucfirst_user($str, $key) {
			return ucfirst($str);
		}
		$array = explode('_', get('id'));
		array_walk($array, 'ucfirst_user');
		$id = implode('', $array);
		$file = ERROR . DIRECTORY_SEPARATOR . $id . '.inc';
		
		if(file_exists($file)) {
			ob_start();
			include($file);
			$notice = ob_get_clean();
		} else {
			$notice = "Chybová stránka s daným ID nebyla nalezena";
		}
		
		$this->render('files/View/Empty.inc', array(
				'nadpis' => 'Chyba',
				'notice' => $notice
		));
	}
	function sidebar() { }
}
?>