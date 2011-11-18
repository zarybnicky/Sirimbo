<?php
User::checkPermissionsError(L_EDITOR);
header_main('Správa nástěnky');

notice(View::getRedirectMessage());

$request = explode('/', $_SERVER['REQUEST_URI']);
$action = array_slice($request, 3);
if(empty($action)) $action[] = '';

switch($action[0]) {
	case 'add':
		include('files/Admin/Nastenka/Form.inc');
		return;
	
	case 'edit':
		$id = isset($action[1]) ? $action[1] : '';
		
		if($id && is_numeric($id) && ($data = DBNastenka::getSingleNastenka($id))) {
			if(Permissions::canEditNastenka($data['up_kdo'], $data['up_lock']))
				View::viewError(ER_AUTHORIZATION);
			
			$_POST['id'] = $id;
			$_POST['nadpis'] = $data['up_nadpis'];
			$_POST['text'] = $data['up_text'];
			$_POST['zluta'] = ($data['up_barvy'] & C_ZLUTA);
			$_POST['cervena'] = ($data['up_barvy'] & C_CERVENA);
			$_POST['modra'] = ($data['up_barvy'] & C_MODRA);
			$_POST['lock'] = $data['up_lock'];
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

switch(getPostField('action')) {
	case 'remove':
		if(is_array(getPostField('nastenka'))) {
			foreach(getPostField('nastenka') as $item) {
				$data = DBNastenka::getSingleNastenka($item);
				if(Permissions::canEditNastenka($data['up_kdo'], $data['up_lock']))
					View::viewError(ER_AUTHORIZATION);
					
				DBNastenka::removeNastenka($item);
				DBNovinky::addNovinka('Uživatel ' . User::getUserWholeName() .
					' smazal příspěvek z nástěnky');
			}
			notice('Příspěvky odebrány');
		}
		break;
	
	case 'edit':
		$nastenka = getPostField('nastenka');
		if($nastenka[0]) {
			header('Location: /admin/nastenka/edit/' . $nastenka[0]);
			return;
		}
		break;
	
	case 'edit_confirm':
		$id = getPostField('id');
		$data = DBNastenka::getSingleNastenka($id);
		
		if(Permissions::canEditNastenka($data['up_kdo'], $data['up_lock']))
			View::viewError(ER_AUTHORIZATION);
		
		$zluta = ((bool) getPostField('zluta')) ? C_ZLUTA : 0;
		$cervena = ((bool) getPostField('cervena')) ? C_CERVENA : 0;
		$modra = ((bool) getPostField('modra')) ? C_MODRA : 0;
		$barvy = $zluta | $cervena | $modra;
		
		$lock = (getPostField('lock') == 'lock') ? 1 : 0;
		
		DBNastenka::editNastenka($id, getPostField('nadpis'),
			getPostField('text'), $barvy, (getPostField('lock') == 'lock') ? 1 : 0);
		DBNovinky::addNovinka('Uživatel ' . User::getUserWholeName() . ' upravil příspěvek na nástěnce');
		
		notice('Příspěvek úspěšně upraven');
		break;
		
	case 'add_confirm':
		$id = getPostField('id');
		
		$zluta = ((bool) getPostField('zluta')) ? C_ZLUTA : 0;
		$cervena = ((bool) getPostField('cervena')) ? C_CERVENA : 0;
		$modra = ((bool) getPostField('modra')) ? C_MODRA : 0;
		$barvy = $zluta | $cervena | $modra;
		
		$lock = (getPostField('lock') == 'lock') ? 1 : 0;
		
		DBNastenka::addNastenka(User::getUserID(), getPostField('nadpis'),
			getPostField('text'), $barvy, $lock);
		DBNovinky::addNovinka('Uživatel ' . User::getUserWholeName() . ' přidal příspěvek na nástěnku');
		
		notice('Příspěvek úspěšně přidán');
		break;
}

include('files/Admin/Nastenka/Display.inc');
?>