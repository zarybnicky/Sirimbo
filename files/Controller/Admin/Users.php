<?php
include_once('files/Controller/Admin.php');
class Controller_Admin_Users extends Controller_Admin {
	function __construct() {
		Permissions::checkError('users', P_OWNED);
	}
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
				$groups = DBPermissions::getGroups();
				foreach($groups as $group)
					if($group['pe_id'])
						$filter[] = $group['pe_id'];
				
				$options['filter'] = in_array(get('f'), array_merge(array('dancer', 'system', 'all', 'unconfirmed', 'ban'), $filter)) ?
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
							$user['u_group'], post($id . '-skupina'), post($id . '-dancer') ? '1' : '0',
							$user['u_lock'], $user['u_ban'], post($id . '-system') ? '1' : '0');
					}
				}
				break;
			case 'remove':
				if(!is_array(post('users')))
					break;
				$url = '/admin/users/remove?';
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
		foreach(post('users') as $id)
			DBUser::removeUser($id);
		
		View::redirect('/admin/users', 'Uživatelé odebráni');
	}
	function add($id = null) {
		if(empty($_POST) || is_object($f = $this->checkData($_POST, 'add'))) {
			include('files/Admin/Users/Form.inc');
			return;
		}
		$narozeni = Helper::get()->date()->name('narozeni')->getPost();
		
		DBUser::addUser(strtolower(post('login')), User::Crypt(post('pass')), post('jmeno'), post('prijmeni'),
			post('pohlavi'), post('email'), post('telefon'), $narozeni,
			post('poznamky'), post('group'), post('skupina'), post('dancer') ? '1' : '0',
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
			post('group', $data['u_group']);
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
		if(is_object($f = $this->checkData($_POST, 'edit'))) {
			include('files/Admin/Users/Form.inc');	
			return;
		}
		
		$narozeni = Helper::get()->date()->name('narozeni')->getPost();
		
		DBUser::setUserData($id, post('jmeno'), post('prijmeni'), post('pohlavi'),
			post('email'), post('telefon'), $narozeni, post('poznamky'), post('group'),
			post('skupina'), post('dancer') ? 1 : 0, post('lock') ? 1 : 0,
			post('ban') ? 1 : 0, post('system') ? 1 : 0);
		View::redirect('/admin/users', 'Uživatel úspěšně upraven');
	}
	function platby($id = null) {
		if(!$id || !($data = DBUser::getUserData($id)))
			View::redirect('/admin/users', 'Uživatel s takovým ID neexistuje');
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
				
				DBUser::confirmUser($id, post($id . '-group'), post($id . '-skupina'),
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
	function temporary($id = null) {
		$jmeno = post('jmeno');
		$prijmeni = post('prijmeni');
		$narozeni = post('narozeni');
		$rok = explode('-', $narozeni);
		$rok = array_shift($rok);
		
		$login = preg_replace('/[^a-zA-Z0-9.-_]*/','', strtolower($prijmeni)) . '_' .
				preg_replace('/[^a-zA-Z0-9.-_]*/','', strtolower($jmeno));
		
		if(!($id = DBUser::getUserID($login))) {
			list($user_id, $par_id) = DBUser::addTemporaryUser($login, $jmeno, $prijmeni, $narozeni);
			
			header('Content-Type: application/json');
			echo json_encode(array('user_id' => $user_id, 'par_id' => $par_id, 'jmeno' => $jmeno,
				'prijmeni' => $prijmeni, 'narozeni' => $narozeni, 'rok' => $rok));
		} else {
			$data = DBUser::getUserData($id);
			if(!($partner = DBPary::getLatestPartner($data['u_id'], $data['u_pohlavi']))) {
				$par_id = DBPary::noPartner($user['u_id']);
			} else {
				$par_id = $partner['p_id'];
			}
			$narozeni = explode('-', $data['u_narozeni']);
			
			header('Content-Type: application/json');
			echo json_encode(array('user_id' => $data['u_id'], 'par_id' => $par_id, 'jmeno' => $data['u_jmeno'],
				'prijmeni' => $data['u_prijmeni'], 'narozeni' => $data['u_narozeni'],
				'rok' => array_shift($narozeni)));
		}
		exit;
	}
	private function checkData($data, $action = 'add') {
		$narozeni = Helper::get()->date()->name('narozeni')->getPost();
		
		$f = new Form();
		$f->checkLength(post('jmeno'), 1, 40, 'Špatná délka jména', 'jmeno');
		$f->checkLength(post('prijmeni'), 1, 40, 'Špatná délka přijmení', 'prijmeni');
		$f->checkDate($narozeni, 'Neplatné datum narození', 'narozeni');
		$f->checkInArray(post('pohlavi'), array('m', 'f'), 'Neplatné pohlaví', 'pohlavi');
		$f->checkEmail(post('email'), 'Neplatný formát emailu', 'email');
		$f->checkPhone(post('telefon'), 'Neplatný formát telefoního čísla', 'telefon');
		
		if($action == 'add') {
			$f->checkLogin(post('login'), 'Špatný formát přihlašovacího jména', 'login');
			$f->checkPassword(post('pass'), 'Špatný formát hesla', 'pass');
			$f->checkBool(!DBUser::getUserID(post('login')),
				'Uživatel s takovým přihlašovacím jménem už tu je', 'login');
		}
		return $f->isValid() ? true : $f;
	}
}