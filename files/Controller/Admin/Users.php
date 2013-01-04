<?php
class Controller_Admin_Users implements Controller_Interface {
	
	//FIXME: Permissions!!
	
	function view($id = null) {
		if(empty($_POST)) {
			include('files/Admin/Users/Display.inc');
			return;
		}
		switch(post('action')) {
			case 'edit':
				$users = post('users');
				if($users[0])
					View::redirect('/admin/users/edit/' . $users[0]);
				break;
				
			case 'platby':
				$users = post('users');
				if($users[0])
					View::redirect('/admin/users/platby/' . $users[0]);
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
				break;
			case 'remove': //FIXME:URI Building
				if(!is_array(post('users')))
					break;
				$url = Request::getURI() . '/remove';
				foreach(post('users') as $id)
					$url .= '&u[]=' . $id;
				View::redirect($url);
				break;
		}
		include('files/Admin/Users/Display.inc');
		return;
	}
	function remove($id = null) {
		if(empty($_POST) || post('action') !== 'confirm') {
			include('files/Admin/Users/DisplayRemove.inc');
			return;
		}
		if(!is_array(post('users')))
			View::redirect('/admin/users');
		foreach(post('users') as $id) {
			$data = DBUser::getUserData($id);
			
			if(Permissions::canEditUser($id, $data['u_level'], $data['u_lock']))
				DBUser::removeUser($id);
			else
				$error = true;
		}
		if(isset($error) && $error)
			View::viewError(ER_AUTHORIZATION);
		
		View::redirect('/admin/users', 'Uživatelé odebráni');
	}
	function add($id = null) {
		if(empty($_POST) || !$this->checkData($_POST, 'add')) {
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
		DBUser::addUser(strtolower(post('login')), User::Crypt(post('pass')), post('jmeno'), post('prijmeni'),
			post('pohlavi'), post('email'), post('telefon'), $narozeni,
			post('poznamky'), $level, post('skupina'), post('dancer') ? '1' : '0',
			post('lock') ? '1' : '0', post('ban') ? '1' : '0', '1', post('system') ? '1' : '0');
		View::redirect('/admin/users', 'Uživatel úspěšně přidán');
	}
	function edit($id = null) {
		if(!$id || !($data = DBUser::getUserData($id)))
			View::redirect('/admin/users', 'Uživatel s takovým ID neexistuje');
		if(!$data['u_confirmed'])
			View::redirect('/admin/users',
				'Uživatel "' . $data['u_login'] . '" ještě není potvrzený');
			
		if(empty($_POST)) {
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
		if(!$this->checkData($_POST, 'edit')) {
			include('files/Admin/Users/Form.inc');	
			return;
		}
		//TODO: Test DisplayUsers::transformUserLevel($value, $from, $to);
		//$level = DisplayUsers::transformUserLevel(post('level'),
		//	DisplayUsers::$plaintext, DisplayUsers::$constant);
        
		$narozeni = Helper::get()->date()->name('narozeni')->getPost();
        
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
			post('skupina'), post('dancer') ? 1 : 0, post('lock') ? 1 : 0,
			post('ban') ? 1 : 0, post('system') ? 1 : 0);
		View::redirect('/admin/users', 'Uživatel úspěšně upraven');
	}
	function platby($id = null) {
		if(!$id || !($data = DBUser::getUserData($id)))
			View::redirect('/admin/users', 'Uživatel s takovým ID neexistuje');
		if(!Permissions::canEditUser($id, $data['u_level'], $data['u_lock']))
			View::viewError(ER_AUTHORIZATION);
		if(!$data['u_confirmed'])
			View::redirect('/admin/users',
				'Uživatel "' . $data['u_login'] . '" ještě není potvrzený');
		
		include('files/Admin/Users/DisplayPlatby.inc');
		return;
	}
	function unconfirmed($id = null) {
		if(empty($_POST)) {
			include('files/Admin/Users/DisplayNew.inc');
			return;
		}
		if(post('action') == 'confirm') {
			if(!is_array(post('users'))) {
				include('files/Admin/Users/DisplayNew.inc');
				return;
			}
			foreach(post('users') as $id) {
				$data = DBUser::getUserData($id);
				
				$level = (in_array(post($id . '-level'), array('host', 'user', 'editor', 'trener', 'admin')) ?
					post($id . '-level') : 'host');
				
				DBUser::confirmUser($id, $level, post($id . '-skupina'),
					post($id . '-dancer') ? 1 : 0);
				Mailer::reg_confirmed_notice($data['u_email'], $data['u_login']);
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
				echo '<input type="hidden" name="users[]" value="', $id, '" />';
				echo '<br />';
			}
			echo '<br/>';
			echo '<button type="submit" name="action" value="remove_confirm">Odstranit</button>';
			echo '</form>';
			echo '<a href="/admin/users/new">Zpět</a>';
			return;
		}
	}
	function duplicate($id = null) {
		if(empty($_POST) || post('action') !== 'remove' || !post('users')) {
			include('files/Admin/Users/DisplayDuplicate.inc');
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
		echo '<a href="/admin/users/duplicate">Zpět</a>';
		return;
	}
	private function checkData($data, $action = 'add') {
		$narozeni = Helper::get()->date()->name('narozeni')->getPost();
		
		$f = new Form();
		$f->checkLength(post('jmeno'), 1, 40, 'Špatná délka jména', 'jmeno');
		$f->checkLength(post('prijmeni'), 1, 40, 'Špatná délka přijmení', 'prijmeni');
		$f->checkInArray(post('pohlavi'), array('m', 'f'), 'Neplatné pohlaví', 'pohlavi');
		$f->checkEmail(post('email'), 'Neplatný formát emailu', 'email');
		$f->checkPhone(post('telefon'), 'Neplatný formát telefoního čísla', 'telefon');
		$f->checkDate($narozeni, 'Neplatné datum narození', 'narozeni');
		if($action == 'add') {
			$f->checkLogin(post('login'), 'Špatný formát přihlašovacího jména', 'login');
			$f->checkPassword(post('pass'), 'Špatný formát hesla', 'pass');
			$f->checkBool(DBUser::getUserID(post('login')),
				'Uživatel s takovým přihlašovacím jménem už tu je', 'login');
		}
		return $f->isValid();
	}
}