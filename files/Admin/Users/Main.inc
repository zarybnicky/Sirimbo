<?php
User::checkPermissionsError(L_ADMIN);
header_main('Správa uživatelů');

notice(View::getRedirectMessage());

$request = explode('/', $_SERVER['REQUEST_URI']);
$action = array_slice($request, 3);
if(empty($action)) $action[] = '';

switch($action[0]) {
	case 'add':
		if(empty($_POST)) {
			include('files/Admin/Users/Form.inc');
			return;
		} else {
			$error = false;
			ob_start();
			include('files/Admin/Users/Form.inc');	
			$html = ob_get_clean();
			
			if(!$error) {
				if(DBUser::getUserID(post('login'))) {
					notice('Uživatel s takovým jménem už tu je');
					echo $html;
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
				
				DBUser::addUser(post('login'), $pass, post('jmeno'), post('prijmeni'),
					post('pohlavi'), post('email'), post('telefon'),
					post('poznamky'), $level, post('lock') ? 1 : 0,
					post('ban') ? 1 : 0, '1');
				View::redirect('/admin/users', 'Uživatel úspěšně přidán');
			} else {
				echo $html;
				return;
			}
		}
		break;
	
	case 'edit':
		$id = isset($action[1]) ? $action[1] : '';
		
		if(!$id || !is_numeric($id) || !($data = DBUser::getUserData($id)))
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
			post('jmeno', $data['u_jmeno']);
			post('prijmeni', $data['u_prijmeni']);
			post('pohlavi', $data['u_pohlavi']);
			post('email', $data['u_email']);
			post('telefon', $data['u_telefon']);
			post('poznamky', $data['u_poznamky']);
			include('files/Admin/Users/Form.inc');
			return;
		} else {
			$error = false;
			ob_start();
			include('files/Admin/Users/Form.inc');	
			$html = ob_get_clean();
			
			if(!$error) {
				//TODO: Test DisplayUsers::transformUserLevel($value, $from, $to);
				$level = DisplayUsers::transformUserLevel(post('level'),
					DisplayUsers::$plaintext, DisplayUsers::$constant);
				
				switch(post('level')) {
					case 'host':	$level = L_HOST;	break;
					case 'user':	$level = L_USER;	break;
					case 'editor':	$level = L_EDITOR;	break;
					case 'trener':	$level = L_TRENER;	break;
					case 'admin':	$level = L_ADMIN;	break;
					case 'sadmin':	$level = L_SADMIN;	break;
					default:		$level = L_HOST;	break;
				}
	
				DBUser::setUserData($id, post('jmeno'), post('prijmeni'),
					post('pohlavi'), post('email'), post('telefon'),
					post('poznamky'), $level, post('lock') ? 1 : 0,
					post('ban') ? 1 : 0);
				View::redirect('/admin/users', 'Uživatel úspěšně upraven');
			} else {
				echo $html;
				return;
			}
		}
		break;
		
	case 'new':
		if(empty($_POST)) {
			include('files/Admin/Users/DisplayNew.inc');
			return;
		} else {
			if(post('action') == 'confirm') {
				if(is_array(post('users'))) {
					foreach(post('users') as $id) {
						$old_level = post($id . '-level');
						$level = ($old_level == 'host' || $old_level == 'user' ||
								$old_level == 'editor' || $old_level == 'trener' || $old_level == 'admin') ?
							$old_level : 'host';
						DBUser::confirmUser($id, $level);
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