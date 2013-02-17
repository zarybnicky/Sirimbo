<?php
class Controller_Admin_Rozpis implements Controller_Interface {
	function __construct() {
		Permissions::checkError('rozpis', P_OWNED);
	}
	function view($id = null) {
		if(empty($_POST)) {
			include('files/Admin/Rozpis/Display.inc');
			return;
		}
		switch(post('action')) {
			case 'edit':
				$rozpis = post('rozpis');
				if($rozpis[0])
					View::redirect('/admin/rozpis/edit/' . $rozpis[0]);
				break;
			case 'edit_detail':
				$rozpis = post('rozpis');
				if($rozpis[0])
					View::redirect('/admin/rozpis/detail/' . $rozpis[0]);
				break;
			case 'remove':
				if(!is_array(post('rozpis')))
					View::redirect('/admin/rozpis');
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
					View::viewError(ER_AUTHORIZATION);
				
				View::redirect('/admin/rozpis', 'Rozpisy odebrány');
		}
		
		include('files/Admin/Rozpis/Display.inc');
	}
	function add($id = null) {
		if(empty($_POST) || is_object($f = $this->checkData($_POST, 'add'))) {
			include('files/Admin/Rozpis/Form.inc');
			return;
		}
		Permissions::checkError('rozpis', P_OWNED, post('trener'));
   
		$datum = Helper::get()->date()->name('datum')->getPost();
		
		$visible = (bool) post('visible');
		if(!Permissions::check('rozpis', P_ADMIN) && $visible) {
			$visible = false;
			View::setRedirectMessage('Nemáte dostatečná oprávnění ke zviditelnění příspěvku');
		}
		DBRozpis::addRozpis(post('trener'), post('kde'), $datum, $visible, post('lock'));
		
		if($visible) {
			$trener_data = DBUser::getUserData(post('trener'));
			$trener_name = $trener_data['u_jmeno'] . ' ' . $trener_data['u_prijmeni'];
			
			DBNovinky::addNovinka('Uživatel ' . User::getUserWholeName() . ' přidal rozpis ' .
				formatDate($datum) . ' s trenérem ' . $trener_name);
		}
		View::redirect('/admin/rozpis', 'Rozpis přidán');
	}
	function edit($id = null) {
		if(!$id || !($data = DBRozpis::getSingleRozpis($id)))
			View::redirect('/admin/rozpis', 'Rozpis s takovým ID neexistuje');
		
		Permissions::checkError('rozpis', P_OWNED, $data['r_trener']);
		
		if(empty($_POST)) {
			post('id', $id);
			post('trener', $data['r_trener']);
			post('kde', $data['r_kde']);
			post('datum', $data['r_datum']);
			post('visible', $data['r_visible']);
			post('lock', $data['r_lock']);
			
			include('files/Admin/Rozpis/Form.inc');
			return;
		}
		if(is_object($this->checkData($_POST, 'edit'))) {
			include('files/Admin/Rozpis/Form.inc');
			return;
		}
		$datum = Helper::get()->date()->name('datum')->getPost();
		
		$visible = (bool) post('visible');
		$visible_prev = $data['r_visible'];
		if(!Permissions::check('rozpis', P_ADMIN) && $visible && !$visible_prev) {
			$visible = $visible_prev;
			View::setRedirectMessage('Nemáte dostatečná oprávnění ke zviditelnění rozpisu');
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
		View::redirect('/admin/rozpis', 'Rozpis úspěšně upraven');
	}
	
	private function checkData($data, $action = 'add') {
		$datum = Helper::get()->date()->name('datum')->getPost();
		
		$f = new Form();
		$f->checkNumeric(post('trener'), 'Neplatný trenér', 'trener');
		$f->checkDate($datum, 'Neplatný formát data', 'datum');

		return $f->isValid() ? true : $f;
	}
}
?>