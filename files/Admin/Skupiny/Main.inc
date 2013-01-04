<?php
User::checkPermissionsError(L_ADMIN);
header_main('Správa skupin');

notice(View::getRedirectMessage());

switch(Request::getAction()) {
	case 'add':
		if(empty($_POST)) {
			include('files/Admin/Skupiny/Form.inc');
			return;
		}
		$f = new Form();
		$f->checkBool(post('platba') === '' || is_numeric(post('platba')),
			'Platba musí být zadaná jen čísly', 'platba');
		$f->checkBool(post('platba_ctvrtrok') === '' || is_numeric(post('platba_ctvrtrok')),
			'Platba musí být zadaná jen čísly', 'platba_ctvrtrok');
		$f->checkBool(post('platba_pulrok') === '' || is_numeric(post('platba_pulrok')),
			'Platba musí být zadaná jen čísly', 'platba_pulrok');
		if(!$f->isValid()) {
			include('files/Admin/Skupiny/Form.inc');
			return;
		}
		
		$platba = (post('platba') !== '') ? post('platba') :
			(post('platba_ctvrtrok') !== '' ? 
				floor(post('platba_ctvrtrok') / 2.5) :
			(post('platba_pulrok') !== '' ?
				floor(post('platba_pulrok') / 5) : 0));
		$platba_ctvrtrok = (post('platba_ctvrtrok') !== '') ? post('platba_ctvrtrok') :
			(post('platba') !== '' ? 
				floor(post('platba') * 2.5) :
			(post('platba_pulrok') !== '' ?
				floor(post('platba_pulrok') / 2) : 0));
		$platba_pulrok = (post('platba_pulrok') !== '') ? post('platba_pulrok') :
			(post('platba_ctvrtrok') !== '' ? 
				floor(post('platba_ctvrtrok') * 2) :
			(post('platba') !== '' ?
				floor(post('platba') * 5) : 0));
		
		DBSkupiny::addSkupina(post('color'), post('platba'),
			post('platba_ctvrtrok'), post('platba_pulrok'), post('popis'));
		
		View::redirect('/admin/skupiny', 'Skupina úspěšně přidána');
		break;
	
	case 'edit':
		$id = Request::getID();
		if(!$id || !($data = DBSkupiny::getSingleSkupina($id)))
			View::redirect('/admin/skupiny', 'Skupina s takovým ID neexistuje');
		
		if(empty($_POST)) {
			post('color', $data['us_color']);
			post('platba', $data['us_platba_mesic']);
			post('platba_ctvrtrok', $data['us_platba_ctvrtrok']);
			post('platba_pulrok', $data['us_platba_pulrok']);
			post('popis', $data['us_popis']);
			
			include('files/Admin/Skupiny/Form.inc');
			return;
		}
		$f = new Form();
		$f->checkBool(post('platba') === '' || is_numeric(post('platba')),
			'Platba musí být zadaná jen čísly', 'platba');
		$f->checkBool(post('platba_ctvrtrok') === '' || is_numeric(post('platba_ctvrtrok')),
			'Platba musí být zadaná jen čísly', 'platba_ctvrtrok');
		$f->checkBool(post('platba_pulrok') === '' || is_numeric(post('platba_pulrok')),
			'Platba musí být zadaná jen čísly', 'platba_pulrok');
		if(!$f->isValid()) {
			include('files/Admin/Skupiny/Form.inc');
			return;
		}
		
		$platba = (post('platba') !== '') ? post('platba') :
			(post('platba_ctvrtrok') !== '' ? 
				floor(post('platba_ctvrtrok') / 2.5) :
			(post('platba_pulrok') !== '' ?
				floor(post('platba_pulrok') / 5) : 0));
		$platba_ctvrtrok = (post('platba_ctvrtrok') !== '') ? post('platba_ctvrtrok') :
			(post('platba') !== '' ? 
				floor(post('platba') * 2.5) :
			(post('platba_pulrok') !== '' ?
				floor(post('platba_pulrok') / 2) : 0));
		$platba_pulrok = (post('platba_pulrok') !== '') ? post('platba_pulrok') :
			(post('platba_ctvrtrok') !== '' ? 
				floor(post('platba_ctvrtrok') * 2) :
			(post('platba') !== '' ?
				floor(post('platba') * 5) : 0));
		
		DBSkupiny::editSkupina($id, post('color'), $platba,
			$platba_ctvrtrok, $platba_pulrok, post('popis'));
		
		View::redirect('/admin/skupiny', 'Skupina úspěšně upravena');
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