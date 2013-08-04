<?php
include_once('files/Controller/Admin.php');
class Controller_Admin_Skupiny extends Controller_Admin {
	function __construct() {
		Permissions::checkError('skupiny', P_OWNED);
	}
	function view($id = null) {
		if(empty($_POST)) {
			include('files/Admin/Skupiny/Display.inc');
			return;
		}
		switch(post('action')) {
			case 'edit':
				$skupiny = post('skupiny');
				if($skupiny[0])
					View::redirect('/admin/skupiny/edit/' . $skupiny[0]);
				break;
			case 'remove':
				if(!is_array(post('skupiny')))
					break;
				$url = '/admin/skupiny/remove';
				foreach(post('skupiny') as $id)
					$url .= '&u[]=' . $id;
				View::redirect($url);
				break;
		}
		include('files/Admin/Skupiny/Display.inc');
		return;
	}
	function remove($id = null) {
		if(empty($_POST) || post('action') !== 'confirm') {
			include('files/Admin/Skupiny/DisplayRemove.inc');
			return;
		}
		if(!is_array(post('skupiny')))
			View::redirect('/admin/skupiny');
		foreach(post('skupiny') as $id) {
			DBSkupiny::removeSkupina($id);
		}
		
		View::redirect('/admin/skupiny', 'Skupiny odebrány');
	}
	function add($id = null) {
		if(empty($_POST) || is_object($f = $this->checkData($_POST, 'add'))) {
			include('files/Admin/Skupiny/Form.inc');
			return;
		}
		$mesic = post('platba');
		$ctvrtleti = post('platba_ctvrtrok');
		$pololeti = post('platba_pulrok');
		$this->countPlatby($mesic, $ctvrtleti, $pololeti);
		
		DBSkupiny::addSkupina(post('color'), $mesic, $ctvrtleti, $pololeti,
			post('popis'));
		
		View::redirect('/admin/skupiny', 'Skupina úspěšně přidána');
		break;
	}
	function edit($id = null) {
		if(!$id || !($data = DBSkupiny::getSingleSkupina($id)))
			View::redirect('/admin/skupiny', 'Skupina s takovým ID neexistuje');
		
		if(empty($_POST)) {
			post('color', $data['us_color']);
			post('platba', $data['us_platba_mesic']);
			post('platba_ctvrtrok', $data['us_platba_ctvrtrok']);
			post('platba_pulrok', $data['us_platba_pulrok']);
			post('popis', $data['us_popis']);
			
			include('files/Admin/Skupiny/Form.inc');
			return;
		}
		if(is_object($f = $this->checkData($_POST, 'edit'))) {
			include('files/Admin/Skupiny/Form.inc');
			return;
		}
		
		$mesic = post('platba');
		$ctvrtleti = post('platba_ctvrtrok');
		$pololeti = post('platba_pulrok');
		$this->countPlatby($mesic, $ctvrtleti, $pololeti);
		
		DBSkupiny::editSkupina($id, post('color'), $mesic, $ctvrtleti,
			$pololeti, post('popis'));
		
		View::redirect('/admin/skupiny', 'Skupina úspěšně upravena');
		break;
	}
	
	function checkData($data, $action = 'add') {
		$f = new Form();
		$f->checkBool(post('platba') === '' || is_numeric(post('platba')),
			'Platba musí být zadaná jen čísly', 'platba');
		$f->checkBool(post('platba_ctvrtrok') === '' || is_numeric(post('platba_ctvrtrok')),
			'Platba musí být zadaná jen čísly', 'platba_ctvrtrok');
		$f->checkBool(post('platba_pulrok') === '' || is_numeric(post('platba_pulrok')),
			'Platba musí být zadaná jen čísly', 'platba_pulrok');

		return $f->isValid() ? true : $f;
	}
	function countPlatby(&$mesic, &$ctvrtleti, &$pololeti) {
		$platba_mesic =
			$mesic !== '' ?			$mesic :
			($ctvrtleti !== '' ?	floor($ctvrtleti / 2.5) :
			($pololeti !== '' ?		floor($pololeti / 5) : 0));
		$platba_ctvrtleti =
			$ctvrtleti !== '' ?		$ctvrtleti :
			($mesic !== '' ? 		floor($mesic * 2.5) :
			($pololeti !== '' ?		floor($pololeti / 2) : 0));
		$platba_pololeti =
			$pololeti !== '' ?		$pololeti :
			($ctvrtleti !== '' ?	floor($ctvrtleti * 2) :
			($mesic !== '' ?		floor($mesic * 5) : 0));
		$mesic = $platba_mesic;
		$ctvrtleti = $platba_ctvrtleti;
		$pololeti = $platba_pololeti;
	}
}