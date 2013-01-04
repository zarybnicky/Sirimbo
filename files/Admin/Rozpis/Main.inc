<?php
User::checkPermissionsError(L_TRENER);
header_main('Správa rozpisů');

notice(View::getRedirectMessage());

switch(Request::getAction()) {
	case 'add':
		if(empty($_POST)) {
			include('files/Admin/Rozpis/Form.inc');
			return;
		}
		if(!Permissions::canEditRozpis(post('trener')))
			View::viewError(ER_AUTHORIZATION);
   
		$datum = Helper::get()->date()->name('datum')->getPost();
		
		$f = new Form();
		$f->checkNumeric(post('trener'), 'Neplatný trenér', 'trener');
		$f->checkDate($datum, 'Neplatný formát data', 'datum');
		if(!$f->isValid()) {
			include('files/Admin/Rozpis/Form.inc');
			return;
		}
		
		$visible = (bool) post('visible');
		if(User::checkPermissionsBool(L_TRENER) && !User::checkPermissionsBool(L_ADMIN)) {
			$visible = false;
			View::setRedirectMessage('Nemáte dostatečná oprávnění ke zviditelnění příspěvku');
		}
		DBRozpis::addRozpis(post('trener'), post('kde'), $datum, $visible,
			post('lock'));
		
		if($visible) {
			$trener_data = DBUser::getUserData(post('trener'));
			$trener_name = $trener_data['u_jmeno'] . ' ' . $trener_data['u_prijmeni'];
			
			DBNovinky::addNovinka('Uživatel ' . User::getUserWholeName() . ' přidal rozpis ' .
				formatDate($datum) . ' s trenérem ' . $trener_name);
		}
		View::redirect('/admin/rozpis', 'Rozpis přidán');
		break;
	
	case 'edit':
		$id = Request::getID();
		if(!$id || !($data = DBRozpis::getSingleRozpis($id)))
			View::redirect('/admin/rozpis', 'Rozpis s takovým ID neexistuje');
		
		if(!Permissions::canEditRozpis($data['r_trener']))
			View::viewError(ER_AUTHORIZATION);
		
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
		
		$datum = Helper::get()->date()->name('datum')->getPost();
		
		$f = new Form();
		$f->checkNumeric(post('trener'), 'Neplatný trenér', 'trener');
		$f->checkDate($datum, 'Neplatný formát data', 'datum');
		if(!$f->isValid()) {
			include('files/Admin/Rozpis/Form.inc');
			return;
		}

		$visible = (bool) post('visible');
		$visible_prev = $data['r_visible'];
		if(User::checkPermissionsBool(L_TRENER) && !User::checkPermissionsBool(L_ADMIN) &&
				$visible != $visible_prev) {
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
		break;
}

if(empty($_POST)) {
	include('files/Admin/Rozpis/Display.inc');
	return;
}

switch(post('action')) {
	case 'edit':
		$rozpis = post('rozpis');
		if($rozpis[0]) {
			header('Location: /admin/rozpis/edit/' . $rozpis[0]);
			return;
		}
		break;
		
	case 'edit_detail':
		$rozpis = post('rozpis');
		if($rozpis[0]) {
			header('Location: /admin/rozpis/detail/' . $rozpis[0]);
			return;
		}
		break;
	
	case 'remove':
		if(!is_array(post('rozpis'))) {
			include('files/Admin/Rozpis/Display.inc');
			return;
		}
		foreach(post('rozpis') as $item) {
			$trener = DBRozpis::getRozpisTrener($item);
			$data = DBRozpis::getSingleRozpis($item);
			
			if(Permissions::canEditRozpis($trener['u_id'])) {
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
		
		notice('Rozpisy odebrány');
		include('files/Admin/Rozpis/Display.inc');
		return;
}

include('files/Admin/Rozpis/Display.inc');
return;
?>