<?php
User::checkPermissionsError(L_EDITOR);
header_main('Správa nástěnky');

notice(View::getRedirectMessage());

switch(Request::getAction()) {
	case 'add':
		include('files/Admin/Nastenka/Form.inc');
		return;
	
	case 'edit':
		$id = Request::getID();
		if($id && is_numeric($id) && ($data = DBNastenka::getSingleNastenka($id))) {
			if(!Permissions::canEditNastenka($data['up_kdo'], $data['up_lock']))
				View::viewError(ER_AUTHORIZATION);
			$skupiny = DBNastenka::getNastenkaSkupiny($id);
						
			post('id', $id);
			post('nadpis', $data['up_nadpis']);
			post('text', $data['up_text']);
			foreach($skupiny as $skupina) {
				post('sk-' . $skupina['ups_id_skupina'], 1);
			}
			post('lock', $data['up_lock']);
			include('files/Admin/Nastenka/Form.inc');
			return;
		} else {
			View::redirect('/admin/nastenka', 'Nástěnka s takovým ID neexistuje');
		}
		break;
}

if(empty($_POST)) {
	include('files/Admin/Nastenka/Display.inc');
	return;
}

switch(post('action')) {
	case 'remove':
		if(is_array(post('nastenka'))) {
			foreach(post('nastenka') as $item) {
				$data = DBNastenka::getSingleNastenka($item);
				if(!Permissions::canEditNastenka($data['up_kdo'], $data['up_lock']))
					View::viewError(ER_AUTHORIZATION);
					
				DBNastenka::removeNastenka($item);
				DBNovinky::addNovinka('Uživatel ' . User::getUserWholeName() .
					' smazal příspěvek z nástěnky');
			}
			notice('Příspěvky odebrány');
		}
		break;
	
	case 'edit':
		$nastenka = post('nastenka');
		if($nastenka[0]) {
			header('Location: /admin/nastenka/edit/' . $nastenka[0]);
			return;
		}
		break;
	
	case 'edit_confirm':
		$id = post('id');
		$data = DBNastenka::getSingleNastenka($id);
		
		if(!Permissions::canEditNastenka($data['up_kdo'], $data['up_lock']))
			View::viewError(ER_AUTHORIZATION);
		
		$skupiny = array();
		$skupiny_old = DBNastenka::getNastenkaSkupiny($id);
		$skupiny_vse = DBSkupiny::getSkupiny();
		
		foreach($skupiny_old as $skupina) {
			$skupiny[$skupina['ups_id_skupina']] = $skupina['ups_id'];
		}
		$skupiny_old = $skupiny;
		unset($skupiny);
		
		foreach($skupiny_vse as $skupina) {
			if(post('sk-' . $skupina['us_id']) && isset($skupiny_old[$skupina['us_id']])) {
				continue;
			} elseif(post('sk-' . $skupina['us_id']) && !isset($skupiny_old[$skupina['us_id']])) {
				DBNastenka::addNastenkaSkupina($id, $skupina['us_id'],
					$skupina['us_color'], $skupina['us_popis']);
			} elseif(!post('sk-' . $skupina['us_id']) && isset($skupiny_old[$skupina['us_id']])) {
				DBNastenka::removeNastenkaSkupina($skupiny_old[$skupina['us_id']]);
			}
		}
		
		DBNastenka::editNastenka($id, post('nadpis'), post('text'), (post('lock') == 'lock') ? 1 : 0);
		DBNovinky::addNovinka('Uživatel ' . User::getUserWholeName() . ' upravil příspěvek na nástěnce');
		
		notice('Příspěvek úspěšně upraven');
		break;
		
	case 'add_confirm':
		$id = post('id');
		
		$barvy = $zluta | $cervena | $modra;
		
		$lock = (post('lock') == 'lock') ? 1 : 0;
		
		DBNastenka::addNastenka(User::getUserID(), post('nadpis'), post('text'), $lock);
		DBNovinky::addNovinka('Uživatel ' . User::getUserWholeName() . ' přidal příspěvek na nástěnku');
		
		notice('Příspěvek úspěšně přidán');
		break;
}

include('files/Admin/Nastenka/Display.inc');
?>