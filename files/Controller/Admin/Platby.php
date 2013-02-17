<?php
class Controller_Admin_Platby implements Controller_Interface {
	function __construct() {
		Permissions::checkError('platby', P_OWNED);
	}
	function view($id = null) {
		if(empty($_POST)) {
			include('files/Admin/Platby/Display.inc');
			return;
		}
		switch(post('action')) {
			case 'edit':
				$platby = post('platby');
				if($platby[0])
					View::redirect('/admin/platby/edit/' . $platby[0]);
				break;
			case 'remove':
				if(!is_array(post('platby')))
					break;
				$url = '/admin/platby/remove?';
				foreach(post('platby') as $id)
					$url .= '&u[]=' . $id;
				View::redirect($url);
				break;
		}
		
		include('files/Admin/Platby/Display.inc');
	}
	function add($id = null) {
		if(empty($_POST) || is_object($f = $this->checkData($_POST, 'add'))) {
			include('files/Admin/Platby/Form.inc');
			return;
		}
		$placeno = Helper::get()->date()->name('placeno')->getPost();
		$platby = DBPlatby::getPlatbyFromUser(post('user'));
		
		$obdobi = post('obdobi');
		list($year, $month, $day) = explode('-', $placeno);
		
		$plati_od = $year . Settings::$platby_obdobi[$obdobi][0];
		if(strcmp($placeno, $year . '-06-30') < 0 &&
				$obdobi == '1-ctvrtleti') {
			$year = (string) (((int) $year) - 1);
		} elseif(strcmp($placeno, $year . '-07-01') >= 0 &&
				($obdobi == '1-pololeti' || $obdobi == '2-pololeti' ||
				$obdobi == '2-ctvrtleti' || $obdobi == '3-ctvrtleti' ||
				$obdobi == '4-ctvrtleti')) {
			$year = (string) (((int) $year) + 1);
		}
		$plati_do = $year . Settings::$platby_obdobi[$obdobi][1];
		
		foreach($platby as $platba) {
			if($platba['up_obdobi'] != $obdobi)
				continue;
			
			DBPlatby::editPlatba($platba['up_id'], post('user'), $obdobi,
				post('castka') + $platba['up_castka'], $placeno, $plati_od,
				$plati_do);
			
			View::redirect(post('referer') ? post('referer') : '/admin/platby',
				'Druhá platba za stejné období - sloučeno!');
		}
		
		DBPlatby::addPlatba(post('user'), $obdobi, post('castka'), $placeno, $plati_od, $plati_do);
		
		View::redirect(post('referer') ? post('referer') : '/admin/platby',
			'Platba úspěšně přidána');
	}
	function edit($id = null) {
		if(!$id || !($data = DBPlatby::getSinglePlatba($id)))
			View::redirect(post('referer') ? post('referer') : '/admin/platby',
				'Platba s takovým ID neexistuje');
		if(empty($_POST)) {
			post('user', $data['up_id_user']);
			post('castka', $data['up_castka']);
			post('obdobi', $data['up_obdobi']);
			post('placeno', $data['up_placeno']);
			
			include('files/Admin/Platby/Form.inc');
			return;
		}
		if(is_object($f = $this->checkData($_POST, 'edit'))) {
			include('files/Admin/Platby/Form.inc');
			return;
		}
		$placeno = Helper::get()->date()->name('placeno')->getPost();
		$platby = DBPlatby::getPlatbyFromUser(post('user'));
		
		$obdobi = post('obdobi');
		list($year, $month, $day) = explode('-', $placeno);
		
		$plati_od = $year . Settings::$platby_obdobi[$obdobi][0];
		if(strcmp($placeno, $year . '-06-30') < 0 &&
				$obdobi == '1-ctvrtleti') {
			$year = (string) (((int) $year) - 1);
		} elseif(strcmp($placeno, $year . '-07-01') >= 0 &&
				($obdobi == '1-pololeti' || $obdobi == '2-pololeti' ||
				$obdobi == '2-ctvrtleti' || $obdobi == '3-ctvrtleti' ||
				$obdobi == '4-ctvrtleti')) {
			$year = (string) (((int) $year) + 1);
		}
		$plati_do = $year . Settings::$platby_obdobi[$obdobi][1];
	
		foreach($platby as $platba) {
			if($platba['up_obdobi'] != $obdobi || $platba['up_id'] == $id)
				continue;
			
			DBPlatby::editPlatba($platba['up_id'], post('user'), $obdobi,
				post('castka') + $platba['up_castka'], $placeno, $plati_od, $plati_do);
			DBPlatby::removePlatba($id);
			
			View::redirect(post('referer') ? post('referer') : '/admin/platby',
				'Druhá platba za stejné období - sloučeno!');
		}
		
		DBPlatby::editPlatba($id, post('user'), $obdobi, post('castka'),
			$placeno, $plati_od, $plati_do);
		
		View::redirect(post('referer') ? post('referer') : '/admin/platby',
			'Platba úspěšně upravena');
	}
	function remove($id = null) {
		if(empty($_POST) || post('action') !== 'confirm') {
			include('files/Admin/Platby/DisplayRemove.inc');
			return;
		}
		if(!is_array(post('platby')))
			View::redirect('/admin/platby');
		foreach(post('platby') as $id) {
			DBPlatby::removePlatba($id);
		}
		
		View::redirect('/admin/platby', 'Platby odebrány');
	}
	
	private function checkData($data, $action = 'add') {
		$placeno = Helper::get()->date()->name('placeno')->getPost();
		
		$f = new Form();
		$f->checkNumeric(post('user'), 'Neplatný uživatel', 'user');
		$f->checkNumeric(post('castka'), 'Částka musí být zadaná jen čísly', 'castka');
		$f->checkDate($placeno, 'Neplatný formát data', 'placeno');

		return $f->isValid() ? true : $f;
	}
}
?>