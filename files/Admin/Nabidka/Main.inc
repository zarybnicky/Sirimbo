<?php
User::checkPermissionsError(L_TRENER);
header_main('Správa nabídek');

notice(View::getRedirectMessage());

switch(Request::getAction()) {
	case 'add':
		if(empty($_POST))
			include('files/Admin/Nabidka/Form.inc');
			return;
		if(!Permissions::canEditNabidka(post('trener')))
			View::viewError(ER_AUTHORIZATION);
		
		$od = Helper::get()->date()->name('od')->getPost();
		$do = Helper::get()->date()->name('do')->getPost();
		if(!$do || strcmp($od, $do) > 0)
			$do = $od;
		
		$f = new Form();
		$f->checkNumeric(post('trener'), 'ID trenéra musí být číselné', 'trener');
		$f->checkNumeric(post('pocet_hod'), 'Počet hodin prosím zadejte čísly', 'pocet_hod');
		$f->checkDate($od, 'Zadejte prosím platné datum ("Od")', 'od');
		$f->checkDate($do, 'Zadejte prosím platné datum ("Do")', 'do');
		
		if(!$f->isValid()) {
			include('files/Admin/Nabidka/Form.inc');
			return;
		}
		
		$visible = (bool) post('visible');
		if(User::checkPermissionsBool(L_TRENER) && !User::checkPermissionsBool(L_ADMIN)) {
			$visible = false;
			View::setRedirectMessage('Nemáte dostatečná oprávnění ke zviditelnění příspěvku');
		}
		
		DBNabidka::addNabidka(post('trener'), post('pocet_hod'), $od, $do, $visible,
			post('lock') ? 1 : 0);
		
		if($visible) {
			$trener_data = DBUser::getUserData(post('trener'));
			$trener_name = $trener_data['u_jmeno'] . ' ' . $trener_data['u_prijmeni'];
			
			DBNovinky::addNovinka('Uživatel ' . User::getUserWholeName() . ' přidal nabídku ' .
				formatDate($od) . (($od != $do) ?
				(' - ' . formatDate($do)) : '') . ' s trenérem ' . $trener_name);
		}
		
		View::redirect(getReturnURI('/admin/nabidka'), 'Nabídka přidána');
		break;
	
	case 'edit':
		$id = Request::getID();
		if(!$id || !($data = DBNabidka::getSingleNabidka($id)))
			View::redirect(getReturnURI('/admin/nabidka'), 'Nabídka s takovým ID neexistuje');
		
		if(!Permissions::canEditNabidka($data['n_trener']))
			View::viewError(ER_AUTHORIZATION);
			
		if(empty($_POST)) {
			post('id', $id);
			post('trener', $data['n_trener']);
			post('pocet_hod', $data['n_pocet_hod']);
			post('od', $data['n_od']);
			post('do', $data['n_do']);
			post('visible', $data['n_visible']);
			post('lock', $data['n_lock']);
			
			include('files/Admin/Nabidka/Form.inc');
			return;
		}
		
		$od = Helper::get()->date()->name('od')->getPost();
		$do = Helper::get()->date()->name('do')->getPost();
		if(!$do || strcmp($od, $do) > 0)
			$do = $od;
		
		$f = new Form();
		$f->checkNumeric(post('trener'), 'ID trenéra musí být číselné', 'trener');
		$f->checkNumeric(post('pocet_hod'), 'Počet hodin prosím zadejte čísly', 'pocet_hod');
		$f->checkDate($od, 'Zadejte prosím platné datum ("Od")', 'od');
		$f->checkDate($do, 'Zadejte prosím platné datum ("Do")', 'do');
		
		if(!$f->isValid()) {
			include('files/Admin/Nabidka/Form.inc');
			return;
		}
		
		$visible = (bool) post('visible');
		$visible_prev = $data['n_visible'];
		if(User::checkPermissionsBool(L_TRENER) && !User::checkPermissionsBool(L_ADMIN) &&
				$visible != $visible_prev) {
			$visible = $visible_prev;
			View::setRedirectMessage('Nemáte dostatečná oprávnění ke zviditelnění nabídky');
		}
		
		$items = DBNabidka::getNabidkaItemLessons($id);
		$pocet_hod = post('pocet_hod');
		if($pocet_hod < $items) {
			$pocet_hod = $items;
			View::setRedirectMessage('Obsazených hodin už je víc než jste zadali, ' .
				'nemůžu dál snížit počet hodin');
		}
		
		DBNabidka::editNabidka($id, post('trener'), $pocet_hod, $od, $do, $visible,
			post('lock'));
		
		if($visible)
			if(!$visible_prev)
				$act = 'přidal';
			else
				$act = 'upravil';
		elseif(!$visible && $visible_prev && strcmp($data['n_od'], date('Y-m-d')) > 0)
			$act = 'zrušil';
		
		$trener_data = DBUser::getUserData(post('trener'));
		if(isset($act))
			DBNovinky::addNovinka('Uživatel ' . User::getUserWholeName() . ' ' . $act . ' nabídku ' .
				formatDate($od) .
				(($data['n_od'] != $data['n_do']) ? (' - ' . formatDate($data['n_do'])) : '') .
				' s trenérem ' . $trener_data['u_jmeno'] . ' ' . $trener_data['u_prijmeni']);
		
		View::redirect(getReturnURI('/admin/nabidka'), 'Nabídka úspěšně upravena');
		break;
}

if(empty($_POST)) {
	include('files/Admin/Nabidka/Display.inc');
	return;
}

switch(post('action')) {
	case 'edit':
		$nabidka = post('nabidka');
		if($nabidka[0]) {
			header('Location: /admin/nabidka/edit/' . $nabidka[0]);
			return;
		}
		break;
		
	case 'edit_detail':
		$nabidka = post('nabidka');
		if($nabidka[0]) {
			header('Location: /admin/nabidka/detail/' . $nabidka[0]);
			return;
		}
		break;
	
	case 'remove':
		if(is_array(post('nabidka'))) {
			foreach(post('nabidka') as $item) {
				$trener = DBNabidka::getNabidkaTrener($item);
				$data = DBNabidka::getSingleNabidka($item);
				
				if(Permissions::canEditNabidka($trener['u_id'])) {
					DBNabidka::removeNabidka($item);
					if(strcmp($data['n_od'], date('Y-m-d')) > 0 && $data['n_visible'])
						DBNovinky::addNovinka('Uživatel ' . User::getUserWholeName() . ' zrušil nabídku ' .
							formatDate($data['n_od']) . (($data['n_od'] != $data['n_do']) ?
							(' - ' . formatDate($data['n_do'])) : '') . ' s trenérem ' .
							$trener['u_jmeno'] . ' ' . $trener['u_prijmeni']);
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
		break;
}
include('files/Admin/Nabidka/Display.inc');
return;
?>