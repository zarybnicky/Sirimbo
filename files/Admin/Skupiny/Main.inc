<?php
User::checkPermissionsError(L_ADMIN);
header_main('Správa skupin');

notice(View::getRedirectMessage());

$request = explode('/', $_SERVER['REQUEST_URI']);
$action = array_slice($request, 3);
if(empty($action)) $action[] = '';

switch($action[0]) {
	case 'add':
		if(empty($_POST)) {
			include('files/Admin/Skupiny/Form.inc');
			return;
		} else {
			$error = false;
			ob_start();
			include('files/Admin/Skupiny/Form.inc');	
			$html = ob_get_clean();
			
			if(!$error) {
				DBSkupiny::addSkupina(post('color'), post('platba'), post('popis'));
				
				View::redirect('/admin/skupiny', 'Skupina úspěšně přidána');
			} else {
				echo $html;
				return;
			}
		}
		break;
	
	case 'edit':
		$id = isset($action[1]) ? $action[1] : '';
		
		if(!$id || !is_numeric($id) || !($data = DBSkupiny::getSingleSkupina($id)))
			View::redirect('/admin/skupiny', 'Skupina s takovým ID neexistuje');
		
		if(empty($_POST)) {
			post('color', $data['us_color']);
			post('platba', $data['us_platba_mesic']);
			post('popis', $data['us_popis']);
			
			include('files/Admin/Skupiny/Form.inc');
			return;
		} else {
			$error = false;
			ob_start();
			include('files/Admin/Skupiny/Form.inc');	
			$html = ob_get_clean();
			
			if(!$error) {
				DBSkupiny::editSkupina($id, post('color'), post('platba'), post('popis'));
				
				View::redirect('/admin/skupiny', 'Skupina úspěšně upravena');
			} else {
				echo $html;
				return;
			}
		}
		break;
}

if(empty($_POST)) {
	include('files/Admin/Skupiny/Display.inc');
	return;
}

switch(post('action')) {
	case 'edit':
		$skupiny = post('skupiny');
		if($skupiny[0]) {
			header('Location: /admin/skupiny/edit/' . $skupiny[0]);
			return;
		}
		break;
	case 'remove':
		if(!is_array(post('skupiny'))) {
			include('files/Admin/Skupiny/Display.inc');
			return;
		}
		echo '<form action="/admin/skupiny" method="POST">';
		echo 'Opravdu chcete odstranit skupiny:<br/><br/>';
		foreach(post('skupiny') as $id) {
			$data = DBSkupiny::getSingleSkupina($id);
			echo "\t", $data['us_popis'];
			echo '<br />';
			echo '<input type="hidden" name="skupiny[]" value="', $id, '" />';
		}
		echo '<br/>';
		echo '<button type="submit" name="action" value="remove_confirm">Odstranit</button>';
		echo '</form>';
		echo '<a href="/admin/skupiny">Zpět</a>';
		return;

	case 'remove_confirm':
		if(!is_array(post('skupiny'))) {
			include('files/Admin/Skupiny/Display.inc');
			return;
		}
		foreach(post('skupiny') as $id) {
			DBSkupiny::removeSkupina($id);
		}
		
		notice('Skupiny odebrány');
		include('files/Admin/Skupiny/Display.inc');
		return;
}

include('files/Admin/Skupiny/Display.inc');
?>