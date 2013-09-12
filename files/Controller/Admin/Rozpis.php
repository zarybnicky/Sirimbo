<?php
include_once('files/Controller/Admin.php');
class Controller_Admin_Rozpis extends Controller_Admin {
	function __construct() {
		Permissions::checkError('rozpis', P_OWNED);
	}
	function view($id = null) {
		switch(post('action')) {
			case 'save':
				$items = DBRozpis::getRozpis();
				foreach($items as $item) {
					$id = $item['r_id'];
					if((bool) post($id) !== (bool) $item['r_visible'] &&
							Permissions::check('rozpis', P_OWNED, $item['r_trener'])) {
						if(!Permissions::check('rozpis', P_ADMIN) && post($id) && !$item['r_visible']) {
							$this->redirect()->setMessage('Nemáte dostatečná oprávnění ke zviditelnění rozpisu');
						} else {
							DBRozpis::editRozpis($id, $item['r_trener'], $item['r_kde'],
								$item['r_datum'], post($id) ? '1' : '0', $item['r_lock'] ? '1' : '0');
						}
					}
				}
				break;
			case 'edit':
				$rozpis = post('rozpis');
				if($rozpis[0])
					$this->redirect('/admin/rozpis/edit/' . $rozpis[0]);
				break;
			case 'edit_detail':
				$rozpis = post('rozpis');
				if($rozpis[0])
					$this->redirect('/admin/rozpis/detail/' . $rozpis[0]);
				break;
			case 'remove':
				if(!is_array(post('rozpis')))
					$this->redirect('/admin/rozpis');
				foreach(post('rozpis') as $item) {
					$trener = DBRozpis::getRozpisTrener($item);
					$data = DBRozpis::getSingleRozpis($item);
					
					if(Permissions::check('rozpis', P_OWNED, $trener['u_id'])) {
						DBRozpis::removeRozpis($item);
						if(strcmp($data['r_datum'], date('Y-m-d')) > 0 && $data['r_visible'])
							DBNovinky::addNovinka('Uživatel ' . User::getUserWholeName() . ' zrušil rozpis ' .
								formatDate($data['r_datum']) . ' s trenérem ' .
								$trener['u_jmeno'] . ' ' . $trener['u_prijmeni']);
					} else {
						$error = true;
					}
				}
				if(isset($error) && $error)
					throw new Exception("Máte nedostatečnou autorizaci pro tuto akci!");
				
				$this->redirect('/admin/rozpis', 'Rozpisy odebrány');
		}
		$data = DBRozpis::getRozpis();
		foreach($data as &$row) {
			$new_data = array(
					'canEdit' => Permissions::check('rozpis', P_OWNED, $row['r_trener']),
					'fullName' => $row['u_jmeno'] . ' ' . $row['u_prijmeni'],
					'datum' => formatDate($row['r_datum']),
					'kde' => $row['r_kde']
			);
			if($new_data['canEdit'])
				$new_data['checkBox'] = '<input type="checkbox" name="rozpis[]" value="' . $row['r_id'] . '" />';
			else
				$new_data['checkBox'] = '<input type="checkbox" name="rozpis[]" value="" disabled="d" />';
			if(Permissions::check('rozpis', P_ADMIN))
				$new_data['visible'] = getCheckbox($row['r_id'], '1', $row['r_visible']);
			else
				$new_data['visible'] = '&nbsp;' . ($row['r_visible'] ? '&#10003;' : '&#10799;');
			$row = $new_data;
		}
		$this->render('files/View/Admin/Rozpis/Overview.inc', array(
				'showMenu' => !TISK,
				'data' => $data
		));
		return;
	}
	function add($id = null) {
		if(empty($_POST) || is_object($f = $this->checkData($_POST, 'add'))) {
			if(!empty($_POST))
				$this->redirect()->setMessage($f->getMessages());
			$this->render('files/View/Admin/Rozpis/Form.inc', array(
					'action' => Request::getAction(),
					'isAdmin' => Permissions::check('rozpis', P_ADMIN)
			));
			return;
		}
		Permissions::checkError('rozpis', P_OWNED, post('trener'));
		$datum = $this->date('datum')->getPost();
		$visible = (bool) post('visible');
		
		if(!Permissions::check('rozpis', P_ADMIN) && $visible) {
			$visible = false;
			$this->redirect()->setMessage('Nemáte dostatečná oprávnění ke zviditelnění příspěvku');
		}
		DBRozpis::addRozpis(post('trener'), post('kde'), $datum, $visible, post('lock'));
		
		if($visible) {
			$trener_data = DBUser::getUserData(post('trener'));
			$trener_name = $trener_data['u_jmeno'] . ' ' . $trener_data['u_prijmeni'];
			
			DBNovinky::addNovinka('Uživatel ' . User::getUserWholeName() . ' přidal rozpis ' .
				formatDate($datum) . ' s trenérem ' . $trener_name);
		}
		$this->redirect('/admin/rozpis', 'Rozpis přidán');
	}
	function edit($id = null) {
		if(!$id || !($data = DBRozpis::getSingleRozpis($id)))
			$this->redirect('/admin/rozpis', 'Rozpis s takovým ID neexistuje');
		Permissions::checkError('rozpis', P_OWNED, $data['r_trener']);
		
		if(empty($_POST) || is_object($this->checkData($_POST, 'edit'))) {
			if(empty($_POST)) {
				post('id', $id);
				post('trener', $data['r_trener']);
				post('kde', $data['r_kde']);
				post('datum', $data['r_datum']);
				post('visible', $data['r_visible']);
				post('lock', $data['r_lock']);
			} else {
				$this->redirect()->setMessage($f->getMessages());
			}
			$this->render('files/View/Admin/Rozpis/Form.inc', array(
					'action' => Request::getAction(),
					'isAdmin' => Permissions::check('rozpis', P_ADMIN)
			));
			return;
		}
		$datum = $this->date('datum')->getPost();
		
		$visible = (bool) post('visible');
		$visible_prev = $data['r_visible'];
		if(!Permissions::check('rozpis', P_ADMIN) && $visible && !$visible_prev) {
			$visible = $visible_prev;
			$this->redirect()->setMessage('Nemáte dostatečná oprávnění ke zviditelnění rozpisu');
		}
		DBRozpis::editRozpis($id, post('trener'), post('kde'), $datum, $visible,
			post('lock'));
		
		$trener_data = DBUser::getUserData(post('trener'));
		$trener_name = $trener_data['u_jmeno'] . ' ' . $trener_data['u_prijmeni'];
		if($visible) {
			if(!$visible_prev)
				DBNovinky::addNovinka('Uživatel ' . User::getUserWholeName() . ' přidal rozpis ' .
					formatDate($datum) . ' s trenérem ' . $trener_name);
			else
				DBNovinky::addNovinka('Uživatel ' . User::getUserWholeName() . ' upravil rozpis ' .
					formatDate($datum) . ' s trenérem ' . $trener_name);
		} elseif(!$visible && $visible_prev && strcmp($datum, date('Y-m-d')) > 0) {
				DBNovinky::addNovinka('Uživatel ' . User::getUserWholeName() . ' zrušil rozpis ' .
					formatDate($datum) . ' s trenérem ' . $trener_name);
		}
		$this->redirect('/admin/rozpis', 'Rozpis úspěšně upraven');
	}
	
	private function checkData($data, $action = 'add') {
		$datum = $this->date('datum')->getPost();
		
		$f = new Form();
		$f->checkNumeric(post('trener'), 'Neplatný trenér', 'trener');
		$f->checkDate($datum, 'Neplatný formát data', 'datum');

		return $f->isValid() ? true : $f;
	}
}
?>