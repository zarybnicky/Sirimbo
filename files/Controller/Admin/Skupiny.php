<?php
include_once('files/Controller/Admin.php');
class Controller_Admin_Skupiny extends Controller_Admin {
	function __construct() {
		Permissions::checkError('skupiny', P_OWNED);
	}
	function view($id = null) {
		switch(post('action')) {
			case 'edit':
				$skupiny = post('skupiny');
				if($skupiny[0])
					$this->redirect('/admin/skupiny/edit/' . $skupiny[0]);
				break;
			case 'remove':
				if(!is_array(post('skupiny')))
					break;
				$this->redirect('/admin/skupiny/remove?' . http_build_query(array('u' => post('skupiny'))));
				break;
			default: break;
		}
		if(get('v') === null)
			get('v', 'info');
		
		$this->displayOverview(get('v'));
	}
	function remove($id = null) {
		if(!is_array(post('data')) && !is_array(get('u')))
			$this->redirect('/admin/skupiny');
		if(!empty($_POST) && post('action') == 'confirm') {
			foreach(post('data') as $id)
				DBSkupiny::removeSkupina($id);
			$this->redirect('/admin/skupiny', 'Skupiny odebrány');
		}
		$data = array();
		foreach(get('u') as $id) {
			$item = DBSkupiny::getSingleSkupina($id);
			$data[] = array(
					'id' => $item['us_id'],
					'text' => $item['us_popis']
			);
		}
		$this->render('files/View/Admin/RemovePrompt.inc', array(
				'header' => 'Správa skupin',
				'prompt' => 'Opravdu chcete odstranit skupiny:',
				'returnURL' => Request::getReferer(),
				'data' => $data
		));
	}
	function add($id = null) {
		if(empty($_POST) || is_object($f = $this->checkData($_POST, 'add'))) {
			if(!empty($_POST))
				$this->redirect()->setRedirectMessage($f->getMessages());
			$this->render('files/View/Admin/Skupiny/Form.inc', array(
					'action' => Request::getAction()
			));
			return;
		}
		$mesic = post('platba');
		$ctvrtleti = post('platba_ctvrtrok');
		$pololeti = post('platba_pulrok');
		$this->countPlatby($mesic, $ctvrtleti, $pololeti);
		DBSkupiny::addSkupina(post('color'), $mesic, $ctvrtleti, $pololeti, post('popis'));
		
		$this->redirect('/admin/skupiny', 'Skupina úspěšně přidána');
	}
	function edit($id = null) {
		if(!$id || !($data = DBSkupiny::getSingleSkupina($id)))
			$this->redirect('/admin/skupiny', 'Skupina s takovým ID neexistuje');
		
		if(empty($_POST) || is_object($f = $this->checkData($_POST, 'edit'))) {
			if(empty($_POST)) {
				post('color', $data['us_color']);
				post('platba', $data['us_platba_mesic']);
				post('platba_ctvrtrok', $data['us_platba_ctvrtrok']);
				post('platba_pulrok', $data['us_platba_pulrok']);
				post('popis', $data['us_popis']);
			} else {
				$this->redirect()->setRedirectMessage($f->getMessages());
			}
			$this->render('files/View/Admin/Skupiny/Form.inc', array(
					'action' => Request::getAction()
			));
			return;
		}
		$mesic = post('platba');
		$ctvrtleti = post('platba_ctvrtrok');
		$pololeti = post('platba_pulrok');
		$this->countPlatby($mesic, $ctvrtleti, $pololeti);

		DBSkupiny::editSkupina($id, post('color'), $mesic, $ctvrtleti,
			$pololeti, post('popis'));
		$this->redirect('/admin/skupiny', 'Skupina úspěšně upravena');
	}

	private function __getUserRenderData($data) {
		foreach($data as &$user) {
			$new_data = array(
					'id' => $user['u_id'],
					'fullName' => $user['u_prijmeni'] . ', ' . $user['u_jmeno'],
					'zaplaceno' => ($user['up_id'] && strcmp($user['up_plati_do'], date('Y-m-d')) >= 0)
			);
			if($new_data['zaplaceno']) {
				if((strpos($user['up_obdobi'], 'pololeti') &&
						$user['up_castka'] != $user['us_platba_pulrok']) ||
					(strpos($user['up_obdobi'], 'ctvrtleti') &&
						$user['up_castka'] != $user['us_platba_ctvrtrok']))
					$new_data['castka'] = '<span style="color:black;font-weight:bold;">' . $user['up_castka'] . ' Kč</span>';
				else
					$new_data['castka'] = '<span style="color:#0B0;">' . $user['up_castka'] . ' Kč</span>';
			} else {
					$new_data['castka'] = '<span><a href="/admin/platby/add?u=' . $user['u_id'] . '">NE</a></span>';
			}
			$new_data['platbaDatum'] = ($new_data['zaplaceno'] ? formatDate($user['up_plati_do']) : '---');
			$user = $new_data;
		}
		return $data;
	}
	private function displayOverview($action) {
		$data = DBSkupiny::getSkupiny(true);
		foreach($data as $key => &$item) {
			if($action == 'users') {
				if(!$item['us_platba_mesic'] && !$item['us_platba_ctvrtrok'] && !$item['us_platba_pulrok']) {
					unset($data[$key]);
					continue;
				}
				$users = DBUser::getUsersBySkupina($item['us_id'], true);
				$users = $this->__getUserRenderData($users);
		
				$new_data = array(
						'header' => (getColorBox($item['us_color'], $item['us_popis']) .
								'&nbsp;&nbsp;' . $item['us_popis'] . ' (' . count($users) . ')'),
						'users' => $users,
						'count' => $item['us_count']
				);
			} else {
				$new_data = array(
						'checkBox' => '<input type="checkbox" name="skupiny[]" value="' . $item['us_id'] .	'" />',
						'colorBox' => getColorBox($item['us_color'], $item['us_popis']),
						'popis' => $item['us_popis'],
						'platbaPololeti' => $item['us_platba_pulrok'] . ' Kč',
						'platbaCtvrtleti' => $item['us_platba_ctvrtrok'] . ' Kč',
						'platbaMesic' => $item['us_platba_mesic'] . ' Kč'
				);
			}
			$item = $new_data;
		}unset($item);
		if($action == 'users') {
			//Sort into two (or more...) columns
			$content = array(array(), array());
			$count = array(0, 0);
			foreach($data as $skupina) {
				$min = 0;
				foreach($count as $k => $i)
					if($i < $count[$min]) $min = $k;
				$content[$min][] = $skupina;
				$count[$min] += $skupina['count'];
			}
			$data = $content;
		}
		$this->render('files/View/Admin/Skupiny/Overview.inc', array(
				'showMenu' => !TISK,
				'viewType' => $action,
				'data' => $data
		));
	}
	private function checkData($data, $action = 'add') {
		$f = new Form();
		$f->checkBool(post('platba') === '' || is_numeric(post('platba')),
			'Platba musí být zadaná jen čísly', 'platba');
		$f->checkBool(post('platba_ctvrtrok') === '' || is_numeric(post('platba_ctvrtrok')),
			'Platba musí být zadaná jen čísly', 'platba_ctvrtrok');
		$f->checkBool(post('platba_pulrok') === '' || is_numeric(post('platba_pulrok')),
			'Platba musí být zadaná jen čísly', 'platba_pulrok');
		$f->checkNotEmpty(post('popis'), 'Skupina musí mít nějaký popis', 'popis');

		return $f->isValid() ? true : $f;
	}
	private function countPlatby(&$mesic, &$ctvrtleti, &$pololeti) {
		$platba_mesic =
			$mesic !== '' ? $mesic :
			($ctvrtleti !== ''	? floor($ctvrtleti / 2.5) :
			($pololeti !== ''	? floor($pololeti / 5) : 0));
		$platba_ctvrtleti =
			$ctvrtleti !== ''	? $ctvrtleti :
			($mesic !== ''		? floor($mesic * 2.5) :
			($pololeti !== ''	? floor($pololeti / 2) : 0));
		$platba_pololeti =
			$pololeti !== ''	? $pololeti :
			($ctvrtleti !== ''	? floor($ctvrtleti * 2) :
			($mesic !== ''		? floor($mesic * 5) : 0));
		$mesic = $platba_mesic;
		$ctvrtleti = $platba_ctvrtleti;
		$pololeti = $platba_pololeti;
	}
}