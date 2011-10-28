<?php
User::checkPermissionsError(L_EDITOR);
header_main('Správa nástěnky');

$request = explode('/', $_SERVER['REQUEST_URI']);
$action = array_slice($request, 3);
if(empty($action)) $action[] = '';

if($action[0] == 'add') {
	include('files/Admin/Nastenka/Form.inc');
	return;
} elseif($action[0] == 'edit' && is_numeric($action[1])) {
	$id = $action[1];
	
	if($id) {
		$data = DBNastenka::getSingleNastenka($id);
		
		if(!User::checkPermissionsBool(L_ADMIN) &&
				(User::getUserID() != $data['up_kdo'] || $data['up_lock']))
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
	}
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
				if(!User::checkPermissionsBool(L_ADMIN) &&
						(User::getUserID() != $data['up_kdo'] || !$data['up_lock']))
					View::viewError(ER_AUTHORIZATION);
					
				DBNastenka::removeNastenka($item);
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
		
		if(!User::checkPermissionsBool(L_ADMIN) && 
				(User::getUserID() != $data['up_kdo'] || $data['up_lock']))
			break;
		
		$zluta = ((bool) getPostField('zluta')) ? C_ZLUTA : 0;
		$cervena = ((bool) getPostField('cervena')) ? C_CERVENA : 0;
		$modra = ((bool) getPostField('modra')) ? C_MODRA : 0;
		$barvy = $zluta | $cervena | $modra;
		
		$lock = (getPostField('lock') == 'lock') ? 1 : 0;
		
		$error = false;
		ob_start();
		include('files/Admin/Nastenka/Form.inc');	
		$html = ob_get_clean();
		
		if(!$error) {
			DBNastenka::editNastenka($id, getPostField('nadpis'),
				getPostField('text'), $barvy, (getPostField('lock') == 'lock') ? 1 : 0);
			notice('Příspěvek úspěšně upraven');
		} else {
			header('Location: /admin/nastenka/edit/' . $id);
			return;
		}
		break;
	case 'add_confirm':
		$id = getPostField('id');
		
		$zluta = ((bool) getPostField('zluta')) ? C_ZLUTA : 0;
		$cervena = ((bool) getPostField('cervena')) ? C_CERVENA : 0;
		$modra = ((bool) getPostField('modra')) ? C_MODRA : 0;
		$barvy = $zluta | $cervena | $modra;
		
		$lock = (getPostField('lock') == 'lock') ? 1 : 0;
		
		$error = false;
		ob_start();
		include('files/Admin/Nastenka/Form.inc');	
		$html = ob_get_clean();
		
		if(!$error) {
			DBNastenka::addNastenka(User::getUserID(), getPostField('nadpis'),
				getPostField('text'), $barvy, $lock);
			notice('Příspěvek úspěšně přidán');
		} else {
			echo $html;
			return;
		}
		break;
}

include('files/Admin/Nastenka/Display.inc');
?>