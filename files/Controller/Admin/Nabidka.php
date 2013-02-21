<?php
class Controller_Admin_Nabidka implements Controller_Interface {
	function __construct() {
		Permissions::checkError('nabidka', P_OWNED);
	}
	function view($id = null) {
		if(empty($_POST)) {
			include('files/Admin/Nabidka/Display.inc');
			return;
		}
		switch(post('action')) {
			case 'edit':
				$nabidka = post('nabidka');
				if($nabidka[0])
					View::redirect('/admin/nabidka/edit/' . $nabidka[0]);
				break;
				
			case 'edit_detail':
				$nabidka = post('nabidka');
				if($nabidka[0])
					View::redirect('/admin/nabidka/detail/' . $nabidka[0]);
				break;
			case 'remove':
				if(!is_array(post('nabidka'))) {
					include('files/Admin/Nabidka/Display.inc');
					return;
				}
				foreach(post('nabidka') as $item) {
					$data = DBNabidka::getSingleNabidka($item);
					
					if(Permissions::check('nabidka', P_OWNED, $data['n_trener'])) {
						DBNabidka::removeNabidka($item);
						if(strcmp($data['n_od'], date('Y-m-d')) > 0 && $data['n_visible'])
							DBNovinky::addNovinka('Uživatel ' . User::getUserWholeName() . ' zrušil nabídku ' .
								formatDate($data['n_od']) . (($data['n_od'] != $data['n_do']) ?
								(' - ' . formatDate($data['n_do'])) : '') . ' s trenérem ' .
								$data['u_jmeno'] . ' ' . $data['u_prijmeni']);
					} else {
						$error = true;
					}
				}
				if(isset($error) && $error)
					View::viewError(ER_AUTHORIZATION);
				
				notice('Nabídky odebrány');
				include('files/Admin/Nabidka/Display.inc');
				return;
		}
		include('files/Admin/Nabidka/Display.inc');
	}
	function add($id = null) {
		if(empty($_POST) || is_object($f = $this->checkData($_POST, 'add'))) {
			include('files/Admin/Nabidka/Form.inc');
			return;
        }
		Permissions::checkError('nabidka', P_OWNED, post('trener'));
		
		$od = Helper::get()->date()->name('od')->getPost();
		$do = Helper::get()->date()->name('do')->getPost();
		if(!$do || strcmp($od, $do) > 0)
			$do = $od;
		
		$visible = (bool) post('visible');
		if(!Permissions::check('nabidka', P_ADMIN) && $visible) {
			$visible = false;
			View::setRedirectMessage('Nemáte dostatečná oprávnění ke zviditelnění příspěvku');
		}
		
		DBNabidka::addNabidka(post('trener'), post('pocet_hod'), post('max_pocet_hod'), $od, $do, $visible,
			post('lock') ? 1 : 0);
		
		if($visible) {
			$trener_data = DBUser::getUserData(post('trener'));
			$trener_name = $trener_data['u_jmeno'] . ' ' . $trener_data['u_prijmeni'];
			
			DBNovinky::addNovinka('Uživatel ' . User::getUserWholeName() . ' přidal nabídku ' .
				formatDate($od) . (($od != $do) ?
				(' - ' . formatDate($do)) : '') . ' s trenérem ' . $trener_name);
		}
		
		View::redirect(getReturnURI('/admin/nabidka'), 'Nabídka přidána');
	}
	function edit($id = null) {
		if(!$id || !($data = DBNabidka::getSingleNabidka($id)))
			View::redirect(getReturnURI('/admin/nabidka'), 'Nabídka s takovým ID neexistuje');
		
		Permissions::checkError('nabidka', P_OWNED, $data['n_trener']);
			
		if(empty($_POST)) {
			post('id', $id);
			post('trener', $data['n_trener']);
			post('pocet_hod', $data['n_pocet_hod']);
			post('max_pocet_hod', $data['n_max_pocet_hod']);
			post('od', $data['n_od']);
			post('do', $data['n_do']);
			post('visible', $data['n_visible']);
			post('lock', $data['n_lock']);
			
			include('files/Admin/Nabidka/Form.inc');
			return;
		}
		if(is_object($f = $this->checkData($_POST, 'edit'))) {
			include('files/Admin/Nabidka/Form.inc');
			return;
		}
		
		$od = Helper::get()->date()->name('od')->getPost();
		$do = Helper::get()->date()->name('do')->getPost();
		if(!$do || strcmp($od, $do) > 0)
			$do = $od;
		
		$visible = (bool) post('visible');
		$visible_prev = $data['n_visible'];
		if(!Permissions::check('nabidka', P_ADMIN) &&
				$visible && !$visible_prev) {
			$visible = false;
			View::setRedirectMessage('Nemáte dostatečná oprávnění ke zviditelnění nabídky');
		}
		
		$items = DBNabidka::getNabidkaItemLessons($id);
		$pocet_hod = post('pocet_hod');
		if($pocet_hod < $items) {
			$pocet_hod = $items;
			View::setRedirectMessage('Obsazených hodin už je víc než jste zadali, ' .
				'nemůžu dál snížit počet hodin');
		}
		
		$max_lessons = post('max_pocet_hod');
		$max_lessons_old = DBNabidka::getNabidkaMaxItems($id);
		if($max_lessons < $max_lessons_old) {
			$max_lessons = $max_lessons_old;
			View::setRedirectMessage('Zadaný maximální počet hodin/pár je méně než už je zarezervováno, ' .
				'nemůžu dál snížit maximální počet hodin');
		}
		
		DBNabidka::editNabidka($id, post('trener'), $pocet_hod, $max_lessons, $od, $do, $visible,
			post('lock') ? '1' : '0');
		
		if($visible) {
			if(!$visible_prev)	$act = 'přidal';
			else				$act = 'upravil';
		} elseif(!$visible && $visible_prev && strcmp($data['n_od'], date('Y-m-d')) > 0)
			$act = 'zrušil';
		
		$trener_data = DBUser::getUserData(post('trener'));
		if(isset($act))
			DBNovinky::addNovinka('Uživatel ' . User::getUserWholeName() . ' ' . $act . ' nabídku ' .
				formatDate($od) .
				(($data['n_od'] != $data['n_do']) ? (' - ' . formatDate($data['n_do'])) : '') .
				' s trenérem ' . $trener_data['u_jmeno'] . ' ' . $trener_data['u_prijmeni']);
		
		View::redirect(getReturnURI('/admin/nabidka'), 'Nabídka úspěšně upravena');
	}
	function remove($id = null) {
		
	}
	
	private function checkData($data, $action = 'add') {
		$od = Helper::get()->date()->name('od')->getPost();
		$do = Helper::get()->date()->name('do')->getPost();
		
		$f = new Form();
		$f->checkNumeric(post('trener'), 'ID trenéra musí být číselné', 'trener');
		$f->checkNumeric(post('pocet_hod'), 'Počet hodin prosím zadejte čísly', 'pocet_hod');
		$f->checkNumeric(post('max_pocet_hod'), 'Max. počet hodin prosím zadejte čísly', 'max_pocet_hod');
		$f->checkDate($od, 'Zadejte prosím platné datum ("Od")', 'od');
		if($do) $f->checkDate($do, 'Zadejte prosím platné datum ("Do")', 'do');

		return $f->isValid() ? true : $f;
	}
}
?>