<?php
User::checkPermissionsError(L_ADMIN);
header_main('Správa uživatelů');

notice(View::getRedirectMessage());

switch(Request::getAction()) {
	case 'add':
		if(empty($_POST)) {
			include('files/Admin/Users/Form.inc');
			return;
		}
		$narozeni = Helper::get()->date()->name('narozeni')->getPost();
		
		$f = new Form();
		$f->checkLogin(post('login'), 'Špatný formát přihlašovacího jména', 'login');
		$f->checkPassword(post('pass'), 'Špatný formát hesla', 'pass');
		$f->checkLength(post('jmeno'), 1, 40, 'Špatná délka jména', 'jmeno');
		$f->checkLength(post('prijmeni'), 1, 40, 'Špatná délka přijmení', 'prijmeni');
		$f->checkInArray(post('pohlavi'), array('m', 'f'), 'Neplatné pohlaví', 'pohlavi');
		$f->checkEmail(post('email'), 'Neplatný formát emailu', 'email');
		$f->checkPhone(post('telefon'), 'Neplatný formát telefoního čísla', 'telefon');
		$f->checkDate($narozeni, 'Neplatné datum narození', 'narozeni');
		$f->checkBool(!DBUser::getUserID(post('login')),
			'Uživatel s takovým přihlašovacím jménem už tu je', 'login');
		if(!$f->isValid()) {
			include('files/Admin/Users/Form.inc');	
			return;
		}
		
		switch(post('level')) {
			case 'host':	$level = L_HOST;	break;
			case 'user':	$level = L_USER;	break;
			case 'editor':	$level = L_EDITOR;	break;
			case 'trener':	$level = L_TRENER;	break;
			case 'admin':
			case 'sadmin':	$level = L_ADMIN;	break;
			default:		$level = L_HOST;	break;
		}
		$pass = User::Crypt(post('pass'));
		
		DBUser::addUser(strtolower(post('login')), $pass, post('jmeno'), post('prijmeni'),
			post('pohlavi'), post('email'), post('telefon'), $narozeni,
			post('poznamky'), $level, post('skupina'), post('dancer') ? '1' : '0',
			post('lock') ? '1' : '0', post('ban') ? '1' : '0', '1', post('system') ? '1' : '0');
		View::redirect('/admin/users', 'Uživatel úspěšně přidán');
		break;
	
	case 'edit':
		$id = Request::getID();
		if(!$id || !($data = DBUser::getUserData($id)))
			View::redirect('/admin/users', 'Uživatel s takovým ID neexistuje');
		
		if(!Permissions::canEditUser($id, $data['u_level'], $data['u_lock']))
			View::viewError(ER_AUTHORIZATION);
	
		if(!$data['u_confirmed']) {
			View::redirect('/admin/users',
				'Uživatel "' . $data['u_login'] . '" ještě není potvrzený');
		}
			
		if(empty($_POST)) {
			post('id', $id);
			post('login', $data['u_login']);
			switch($data['u_level']) {
				case L_HOST:	post('level', 'host');	break;
				case L_USER:	post('level', 'user');	break;
				case L_EDITOR:	post('level', 'editor');break;
				case L_TRENER:	post('level', 'trener');break;
				case L_ADMIN:	post('level', 'admin');	break;
				case L_SADMIN:	post('level', 'sadmin');break;
				default:		post('level', 'user');	break;
			}
			post('ban', $data['u_ban']);
			post('lock', $data['u_lock']);
			post('dancer', $data['u_dancer']);
			post('system', $data['u_system']);
			post('jmeno', $data['u_jmeno']);
			post('prijmeni', $data['u_prijmeni']);
			post('pohlavi', $data['u_pohlavi']);
			post('email', $data['u_email']);
			post('telefon', $data['u_telefon']);
			post('narozeni', $data['u_narozeni']);
			post('skupina', $data['u_skupina']);
			post('poznamky', $data['u_poznamky']);
			include('files/Admin/Users/Form.inc');
			return;
		}
		$narozeni = Helper::get()->date()->name('narozeni')->getPost();
		
		$f = new Form();
		$f->checkLogin(post('login'), 'Špatný formát přihlašovacího jména', 'login');
		$f->checkLength(post('jmeno'), 1, 40, 'Špatná délka jména', 'jmeno');
		$f->checkLength(post('prijmeni'), 1, 40, 'Špatná délka přijmení', 'prijmeni');
		$f->checkInArray(post('pohlavi'), array('m', 'f'), 'Neplatné pohlaví', 'pohlavi');
		$f->checkEmail(post('email'), 'Neplatný formát emailu', 'email');
		$f->checkPhone(post('telefon'), 'Neplatný formát telefoního čísla', 'telefon');
		$f->checkDate($narozeni, 'Neplatné datum narození', 'narozeni');
		if(!$f->isValid()) {
			include('files/Admin/Users/Form.inc');	
			return;
		}
		//TODO: Test DisplayUsers::transformUserLevel($value, $from, $to);
		//$level = DisplayUsers::transformUserLevel(post('level'),
		//	DisplayUsers::$plaintext, DisplayUsers::$constant);
		
		switch(post('level')) {
			case 'host':	$level = L_HOST;	break;
			case 'user':	$level = L_USER;	break;
			case 'editor':	$level = L_EDITOR;	break;
			case 'trener':	$level = L_TRENER;	break;
			case 'admin':	$level = L_ADMIN;	break;
			case 'sadmin':	$level = L_SADMIN;	break;
			default:		$level = L_HOST;	break;
		}

		DBUser::setUserData($id, post('jmeno'), post('prijmeni'), post('pohlavi'),
			post('email'), post('telefon'), $narozeni, post('poznamky'), $level,
			post('skupina'), post('dancer') ? '1' : '0', post('lock') ? '1' : '0',
			post('ban') ? '1' : '0', post('system') ? '1' : '0');
		View::redirect('/admin/users', 'Uživatel úspěšně upraven');
		break;
		
	
	case 'platby':
		$id = Request::getID();
		if(!$id || !($data = DBUser::getUserData($id)))
			View::redirect('/admin/users', 'Uživatel s takovým ID neexistuje');
		
		if(!Permissions::canEditUser($id, $data['u_level'], $data['u_lock']))
			View::viewError(ER_AUTHORIZATION);
	
		if(!$data['u_confirmed']) {
			View::redirect('/admin/users',
				'Uživatel "' . $data['u_login'] . '" ještě není potvrzený');
		}
		
		include('files/Admin/Users/DisplayPlatby.inc');
		return;
		
	case 'new':
		if(empty($_POST)) {
			include('files/Admin/Users/DisplayNew.inc');
			return;
		}
		if(post('action') == 'confirm') {
			if(is_array(post('users'))) {
				foreach(post('users') as $id) {
					$data = DBUser::getUserData($id);
					
					$old_level = post($id . '-level');
					$level = ($old_level == 'host' || $old_level == 'user' ||
							$old_level == 'editor' || $old_level == 'trener' || $old_level == 'admin') ?
						$old_level : 'host';
					
					DBUser::confirmUser($id, $level, post($id . '-skupina'),
						post($id . '-dancer') ? 1 : 0);
					Mailer::reg_confirmed_notice($data['u_email'], $data['u_login']);
				}
			}
			notice('Uživatelé potvrzeni');
			include('files/Admin/Users/DisplayNew.inc');
			return;
		} elseif(post('action') == 'remove') {
			echo '<form action="/admin/users" method="POST">';
			echo 'Opravdu chcete odstranit uživatele:<br/><br/>';
			foreach(post('users') as $id) {
				$data = DBUser::getUserData($id);
				echo "\t", $data['u_login'], ' - ';
				echoFullJmeno($data);
				echo '<br />';
				echo '<input type="hidden" name="users[]" value="', $id, '" />';
			}
			echo '<br/>';
			echo '<button type="submit" name="action" value="remove_confirm">Odstranit</button>';
			echo '</form>';
			echo '<a href="/admin/users/new">Zpět</a>';
			return;
		}
		break;
	case 'duplicate':
		if(empty($_POST)) {
			include('files/Admin/Users/DisplayDuplicate.inc');
			return;
		}
		if(post('action') == 'remove') {
			echo '<form action="/admin/users" method="POST">';
			echo 'Opravdu chcete odstranit uživatele:<br/><br/>';
			foreach(post('users') as $id) {
				$data = DBUser::getUserData($id);
				echo "\t", $data['u_login'], ' - ';
				echoFullJmeno($data);
				echo '<br />';
				echo '<input type="hidden" name="users[]" value="', $id, '" />';
			}
			echo '<br/>';
			echo '<button type="submit" name="action" value="remove_confirm">Odstranit</button>';
			echo '</form>';
			echo '<a href="/admin/users/duplicate">Zpět</a>';
			return;
		}
		break;
	case 'ban':
		if(empty($_POST)) {
			include('files/Admin/Users/DisplayBan.inc');
			return;
		}
		if(post('action') == 'edit') {
			$users = post('users');
			if($users[0]) {
				header('Location: /admin/users/edit/' . $users[0]);
				return;
			} else {
				include('files/Admin/Users/DisplayBan.inc');
				return;
			}
		}
		if(post('action') == 'remove') {
			echo '<form action="/admin/users" method="POST">';
			echo 'Opravdu chcete odstranit uživatele:<br/><br/>';
			foreach(post('users') as $id) {
				$data = DBUser::getUserData($id);
				echo "\t", $data['u_login'], ' - ';
				echoFullJmeno($data);
				echo '<br />';
				echo '<input type="hidden" name="users[]" value="', $id, '" />';
			}
			echo '<br/>';
			echo '<button type="submit" name="action" value="remove_confirm">Odstranit</button>';
			echo '</form>';
			echo '<a href="/admin/users/duplicate">Zpět</a>';
			return;
		}
		break;
}

if(empty($_POST)) {
	include('files/Admin/Users/Display.inc');
	return;
}

switch(post('action')) {
	case 'edit':
		$users = post('users');
		if($users[0]) {
			header('Location: /admin/users/edit/' . $users[0]);
			return;
		} else {
			include('files/Admin/Users/Display.inc');
			return;
		}
		break;
		
	case 'platby':
		$users = post('users');
		if($users[0]) {
			header('Location: /admin/users/platby/' . $users[0]);
			return;
		} else {
			include('files/Admin/Users/Display.inc');
			return;
		}
		break;
	
	case 'save':
		$options['filter'] = in_array(get('f'), array('dancer', 'trener', 'editor', 'system', 'all')) ?
			get('f') : 'all';
		$pager = new Paging(new PagingAdapterDBSelect('DBUser', $options));
		$pager->setCurrentPageField('p');
		$pager->setItemsPerPageField('c');
		$pager->setDefaultItemsPerPage(20);
		$pager->setPageRange(5);
		$items = $pager->getItems();
		
		foreach($items as $user) {
			$id = $user['u_id'];
			if(((bool) post($id . '-dancer')) !== ((bool) $user['u_dancer']) ||
					((bool) post($id . '-system')) !== ((bool) $user['u_system']) ||
					(post($id . '-skupina') != $user['u_skupina'])) {
				DBUser::setUserData($id, $user['u_jmeno'], $user['u_prijmeni'], $user['u_pohlavi'],
					$user['u_email'], $user['u_telefon'], $user['u_narozeni'], $user['u_poznamky'],
					$user['u_level'], post($id . '-skupina'), post($id . '-dancer') ? '1' : '0',
					$user['u_lock'], $user['u_ban'], post($id . '-system') ? '1' : '0');
			}
		}
		include('files/Admin/Users/Display.inc');
		break;

	case 'remove':
		if(!is_array(post('users'))) {
			include('files/Admin/Users/Display.inc');
			return;
		}
		echo '<form action="/admin/users" method="POST">';
		echo 'Opravdu chcete odstranit uživatele:<br/><br/>';
		foreach(post('users') as $id) {
			$data = DBUser::getUserData($id);
			echo "\t", $data['u_login'], ' - ';
			echoFullJmeno($data);
			echo '<br />';
			echo '<input type="hidden" name="users[]" value="', $id, '" />';
		}
		echo '<br/>';
		echo '<button type="submit" name="action" value="remove_confirm">Odstranit</button>';
		echo '</form>';
		echo '<a href="/admin/users">Zpět</a>';
		return;

	case 'remove_confirm':
		if(!is_array(post('users'))) {
			include('files/Admin/Users/Display.inc');
			return;
		}
		foreach(post('users') as $id) {
			$data = DBUser::getUserData($id);
			
			if(Permissions::canEditUser($id, $data['u_level'], $data['u_lock']))
				DBUser::removeUser($id);
			else
				$error = true;
		}
		if(isset($error) && $error)
			View::viewError(ER_AUTHORIZATION);
		
		notice('Uživatelé odebráni');
		include('files/Admin/Users/Display.inc');
		return;
}
?>