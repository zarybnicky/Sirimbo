<?php
class Controller_Logout extends Controller_Abstract {
	function view($id = null) {
		User::logout();
		$this->redirect('/home', 'Byli jste úspěšně odhlášeni.');
	}
	function sidebar() { }
}
?>