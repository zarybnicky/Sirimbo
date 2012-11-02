<?php
User::checkPermissionsError(L_TRENER);
header_main('Správa nabídek');

notice(View::getRedirectMessage());

switch(Request::getAction()) {
	case 'add':
		if(empty($_POST)) {
			include('files/Admin/Nabidka/Form.inc');
			return;
		} else {
			if(!Permissions::canEditNabidka(post('trener')))
				View::viewError(ER_AUTHORIZATION);
			
			$pocet_hod = post('pocet_hod');
			
			$od = post('od_year') . '-' . post('od_month') . '-' . post('od_day');
			$do = post('do_year') . '-' . post('do_month') . '-' . post('do_day');
			if($do == '0000-00-00' || strcmp($od, $do) > 0)
				$do = $od;
			
			$visible = (bool) post('visible');
			if(User::checkPermissionsBool(L_TRENER) && !User::checkPermissionsBool(L_ADMIN)) {
				$visible = false;
				View::setRedirectMessage('Nemáte dostatečná oprávnění ke zviditelnění příspěvku');
			}
			
			$error = false;
			ob_start();
			include('files/Admin/Nabidka/Form.inc');	
			$html = ob_get_clean();
			
			if(!$error) {
				DBNabidka::addNabidka(post('trener'), $pocet_hod, $od, $do, $visible,
					post('lock'));
				
				if($visible) {
					$trener_data = DBUser::getUserData(post('trener'));
					$trener_name = $trener_data['u_jmeno'] . ' ' . $trener_data['u_prijmeni'];
					
					DBNovinky::addNovinka('Uživatel ' . User::getUserWholeName() . ' přidal nabídku ' .
						formatDate($od) . (($od != $do) ?
						(' - ' . formatDate($do)) : '') . ' s trenérem ' . $trener_name);
				}
				
				View::redirect('/admin/nabidka', 'Nabídka přidána');
			} else {
				echo $html;
				return;
			}
		}
		break;
	
	case 'edit':
		$id = Request::getID();
		if(!$id || !is_numeric($id) || !($data = DBNabidka::getSingleNabidka($id)))
			View::redirect('/admin/nabidka', 'Nabídka s takovým ID neexistuje');
		
		if(!Permissions::canEditNabidka($data['n_trener']))
			View::viewError(ER_AUTHORIZATION);
			
		if(empty($_POST)) {
			post('id', $id);
			post('trener', $data['n_trener']);
			post('pocet_hod', $data['n_pocet_hod']);
			list($od_year, $od_month, $od_day) = explode('-', $data['n_od']);
			post('od_year', $od_year);
			post('od_month', $od_month);
			post('od_day', $od_day);
			list($do_year, $do_month, $do_day) = explode('-', $data['n_do']);
			post('do_year', $do_year);
			post('do_month', $do_month);
			post('do_day', $do_day);
			post('visible', $data['n_visible']);
			post('lock', $data['n_lock']);
			
			include('files/Admin/Nabidka/Form.inc');
			return;
		} else {
			$items = DBNabidka::getNabidkaItemLessons($id);
			$pocet_hod = post('pocet_hod');
			if($pocet_hod < $items) {
				$pocet_hod = $items;
				View::setRedirectMessage('Obsazených hodin už je víc než jste zadali, ' .
					'nemůžu dál snížit počet hodin');
			}
			
			$od = post('od_year') . '-' . post('od_month') . '-' . post('od_day');
			$do = post('do_year') . '-' . post('do_month') . '-' . post('do_day');
			if($do == '0000-00-00' || strcmp($od, $do) > 0)
				$do = $od;
			
			$visible = (bool) post('visible');
			$visible_prev = $data['n_visible'];
			if(User::checkPermissionsBool(L_TRENER) && !User::checkPermissionsBool(L_ADMIN) &&
					$visible != $visible_prev) {
				$visible = $visible_prev;
				View::setRedirectMessage('Nemáte dostatečná oprávnění ke zviditelnění nabídky');
			}
			
			$error = false;
			ob_start();
			include('files/Admin/Nabidka/Form.inc');	
			$html = ob_get_clean();
			
			if(!$error) {
				DBNabidka::editNabidka($id, post('trener'), $pocet_hod, $od, $do, $visible,
					post('lock'));
				
				$trener_data = DBUser::getUserData(post('trener'));
				$trener_name = $trener_data['u_jmeno'] . ' ' . $trener_data['u_prijmeni'];
				if($visible) {
					if(!$visible_prev)
						DBNovinky::addNovinka('Uživatel ' . User::getUserWholeName() . ' přidal nabídku ' .
							formatDate($od) . (($data['n_od'] != $data['n_do']) ?
							(' - ' . formatDate($data['n_do'])) : '') . ' s trenérem ' . $trener_name);
					else
						DBNovinky::addNovinka('Uživatel ' . User::getUserWholeName() . ' upravil nabídku ' .
							formatDate($od) . (($data['n_od'] != $data['n_do']) ?
							(' - ' . formatDate($data['n_do'])) : '') . ' s trenérem ' . $trener_name);
				} elseif(!$visible && $visible_prev && strcmp($data['n_od'], date('Y-m-d')) > 0) {
						DBNovinky::addNovinka('Uživatel ' . User::getUserWholeName() . ' zrušil nabídku ' .
							formatDate($od) . (($data['n_od'] != $data['n_do']) ?
							(' - ' . formatDate($data['n_do'])) : '') . ' s trenérem ' . $trener_name);
				}
				View::redirect('/admin/nabidka', 'Nabídka úspěšně upravena');
			} else {
				echo $html;
				return;
			}
		}
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