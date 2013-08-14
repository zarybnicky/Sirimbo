<?php
class Controller_Home extends Controller_Abstract {
	function view($id = null) {
		if(NABOR && Request::getURL() == '/')
			$this->redirect()->sendRedirect('/nabor');
		if(Request::getURL() == '/')
			$this->redirect()->sendRedirect('/home');
		
		$this->render('files/View/Main/Home.inc');
	}
	function sidebar() { }
}
?>