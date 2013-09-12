<?php
include_once('files/Controller/Admin.php');
class Controller_Admin_Skupiny extends Controller_Admin {
	function __construct() {
		Permissions::checkError('skupiny', P_OWNED);
	}
	function view($id = null) {
		switch(post('action')) {
			case 'edit':
				$skupiny = post('data');
				if($skupiny[0])
					$this->redirect('/admin/skupiny/edit/' . $skupiny[0]);
				break;
			case 'remove':
				if(!is_array(post('data')))
					break;
				$this->redirect('/admin/skupiny/remove?' . http_build_query(array('u' => post('data'))));
				break;
		}
		$data = DBSkupiny::get();
		foreach($data as $key => &$item) {
			$new_data = array(
					'checkBox' => '<input type="checkbox" name="data[]" value="' . $item['s_id'] .	'" />',
					'colorBox' => getColorBox($item['s_color_text'], $item['s_description']),
					'name' => $item['s_name']
			);
			$item = $new_data;
		}
		$this->render('files/View/Admin/Skupiny/Overview.inc', array(
				'showMenu' => !TISK,
				'data' => $data
		));
	}
	function add($id = null) {
		if(empty($_POST) || is_object($f = $this->checkPost())) {
			if(!empty($_POST)) {
				$this->redirect()->setMessage($f->getMessages());
			}
			$this->render('files/View/Admin/Skupiny/Form.inc', array(
					'action' => 'add'
			));
			return;
		}
		DBSkupiny::insert(post('name'), post('color'), post('desc'));
		$this->redirect('/admin/skupiny', 'Skupina úspěšně přidána');
	}
	function edit($id = null) {
		if(!$id || !($data = DBSkupiny::getSingle($id)))
			$this->redirect('/admin/skupiny', 'Skupina s takovým ID neexistuje');
		
		if(empty($_POST) || is_object($f = $this->checkPost())) {
			if(empty($_POST)) {
				post('name', $data['s_name']);
				post('color', $data['s_color_text']);
				post('popis', $data['s_description']);
			} else {
				$this->redirect()->setMessage($f->getMessages());
			}
			$this->render('files/View/Admin/Skupiny/Form.inc', array(
					'action' => 'edit'
			));
			return;
		}
		DBSkupiny::update($id, post('name'), post('color'), post('desc'));
		$this->redirect('/admin/skupiny', 'Skupina úspěšně upravena');
	}
	function remove($id = null) {
		if(!is_array(post('data')) && !is_array(get('u')))
			$this->redirect('/admin/skupiny');
		
		if(!empty($_POST) && post('action') == 'confirm') {
			foreach(post('data') as $id)
				DBSkupiny::delete($id);
			$this->redirect('/admin/skupiny', 'Skupiny odebrány');
		}
		
		$data = array();
		foreach(get('u') as $id) {
			$item = DBSkupiny::getSingle($id);
			$data[] = array(
					'id' => $item['s_id'],
					'text' => $item['s_name']
			);
		}
		$this->render('files/View/Admin/RemovePrompt.inc', array(
				'header' => 'Správa skupin',
				'prompt' => 'Opravdu chcete odstranit skupiny:',
				'returnURL' => Request::getReferer(),
				'data' => $data
		));
	}
	private function checkPost() {
		$f = new Form();
		
		$f->checkNotEmpty(post('name'), 'Zadejte prosím nějaké jméno.');
		$f->checkNotEmpty(post('desc'), 'Zadejte prosím nějaký popis.');
		$f->checkArrayKey(post('color'), Settings::$barvy, 'Zadejte prosím platnou barvu.');
		
		return $f->isValid() ? true : $f;
	}
}