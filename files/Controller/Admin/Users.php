<?php
include_once('files/Controller/Admin.php');
class Controller_Admin_Users extends Controller_Admin {
	function __construct() {
		Permissions::checkError('users', P_OWNED);
	}
	function view($id = null) {
		switch(post('action')) {
			case 'edit':
				$users = post('users');
				if($users[0])
					$this->redirect('/admin/users/edit/' . $users[0]);
				break;
			case 'platby':
				$users = post('users');
				if($users[0])
					$this->redirect('/admin/users/platby/' . $users[0]);
				break;
			case 'remove':
				if(!is_array(post('users'))) break;
				$this->redirect('/admin/users/remove?' . http_build_query(array('u' => post('users'))));
				break;
			case 'save':
				foreach(post('save') as $user_id) {
					$user = DBUser::getUserData($user_id);
					if(	((bool) post($user_id . '-dancer')) !== ((bool) $user['u_dancer']) ||
						((bool) post($user_id . '-system')) !== ((bool) $user['u_system']) ||
							(post($user_id . '-skupina') != $user['u_skupina'])) {
						DBUser::setUserData($user_id, $user['u_jmeno'], $user['u_prijmeni'], $user['u_pohlavi'],
							$user['u_email'], $user['u_telefon'], $user['u_narozeni'], $user['u_poznamky'],
							$user['u_group'], post($user_id . '-skupina'), post($user_id . '-dancer') ? '1' : '0',
							$user['u_lock'] ? '1' : '0', $user['u_ban'] ? '1' : '0', post($user_id . '-system') ? '1' : '0');
					}
				}
				break;
			case null: break;
		}
		if(get('v') === null)
			get('v', 'info');
		$this->displayOverview(get('v'));
	}
	function remove($id = null) {
		if(!is_array(post('data')) && !is_array(get('u')))
			$this->redirect('/admin/users');
		if(!empty($_POST) && post('action') == 'confirm') {
			foreach(post('data') as $id)
				DBUser::removeUser($id);
			$this->redirect('/admin/users', 'Uživatelé odebráni');
		}
		$data = array();
		foreach(get('u') as $id) {
			$item = DBUser::getUserData($id);
			$data[] = array(
					'id' => $item['u_id'],
					'text' => $item['u_jmeno'] . ' ' . $item['u_prijmeni'] . ' - ' .
						$item['u_login']
			);
		}
		$this->render('files/View/Admin/RemovePrompt.inc', array(
				'header' => 'Správa uživatelů',
				'prompt' => 'Opravdu chcete odstranit uživatele:',
				'returnURL' => Request::getReferer(),
				'data' => $data
		));
	}
	function add($id = null) {
		if(empty($_POST) || is_object($f = $this->checkData($_POST, 'add'))) {
			if(!empty($_POST))
				$this->redirect()->setMessage($f->getMessages());
			$this->displayForm();
			return;
		}
		$narozeni = $this->date('narozeni')->getPost();
		DBUser::addUser(strtolower(post('login')), User::Crypt(post('pass')), post('jmeno'), post('prijmeni'),
			post('pohlavi'), post('email'), post('telefon'), (string) $narozeni,
			post('poznamky'), post('group'), post('skupina'), post('dancer') ? '1' : '0',
			post('lock') ? '1' : '0', post('ban') ? '1' : '0', '1', post('system') ? '1' : '0');
		$this->redirect('/admin/users', 'Uživatel úspěšně přidán');
	}
	function edit($id = null) {
		if(!$id || !($data = DBUser::getUserData($id)))
			$this->redirect('/admin/users', 'Uživatel s takovým ID neexistuje');
		if(!$data['u_confirmed'])
			$this->redirect('/admin/users', 'Uživatel "' . $data['u_login'] . '" ještě není potvrzený');
		
		if(empty($_POST) || is_object($f = $this->checkData($_POST, 'edit'))) {
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
			} else {
				$this->redirect()->setMessage($f->getMessages());
			}
			$this->displayForm();
			return;
		}
		$narozeni = $this->date('narozeni')->getPost();
		DBUser::setUserData($id, post('jmeno'), post('prijmeni'), post('pohlavi'),
			post('email'), post('telefon'), (string) $narozeni, post('poznamky'), post('group'),
			post('skupina'), post('dancer') ? 1 : 0, post('lock') ? 1 : 0,
			post('ban') ? 1 : 0, post('system') ? 1 : 0);
		$this->redirect('/admin/users', 'Uživatel úspěšně upraven');
	}
	function platby($id = null) {
		if(!$id || !($data = DBUser::getUserData($id)))
			$this->redirect('/admin/users', 'Uživatel s takovým ID neexistuje');
		if(!$data['u_confirmed'])
			$this->redirect('/admin/users', 'Uživatel "' . $data['u_login'] . '" ještě není potvrzený');
		
		$platby = DBPlatby::getPlatbyFromUser($id);
		if(empty($platby))
			$this->redirect('/admin/users', 'Uživatel "' . $data['u_login'] . '" nemá žádné platby v databázi.');
		
		foreach($platby as &$item) {
			$new_data = array(
					'id' => $item['up_id'],
					'colorBox' => getColorBox($item['s_color_text'], $item['s_description']),
					'mesicne' => 'TODO!!! Kč',
					'castka' => $item['up_castka'] . ' Kč',
					'datePlaceno' => formatDate($item['up_placeno']),
					'datePlatnost' => formatDate($item['up_plati_do']),
					'upravitLink' => '<a href="/admin/platby/edit/' . $item['up_id'] . '">Upravit</a>'
			);
			$item = $new_data;
		}
		$this->render('files/View/Admin/Users/Platby.inc', array(
				'id' => $id,
				'fullName' => $data['u_jmeno'] . ' ' . $data['u_prijmeni'],
				'platby' => $platby
		));
	}
	function unconfirmed($id = null) {
		if(empty($_POST) || !is_array(post('users'))) {
			$users = DBUser::getNewUsers();
			if(empty($users)) {
				$this->render('files/View/Empty.inc', array(
						'nadpis' => 'Správa uživatelů',
						'notice' => 'Žádní nepotvrzení uživatelé nejsou v databázi.'
				));
			}
			$groups = DBPermissions::getGroups();
			$s_group = $this->select();
			foreach($groups as $group)
				$s_group->option($group['pe_id'], $group['pe_name']);
			
			$skupiny = DBSkupiny::get();
			$s_skupina = new SelectHelper();
			foreach($skupiny as $skupina)
				$s_skupina->option($skupina['s_id'], $skupina['s_name']);
			
			foreach($users as &$row) {
				$new_data = array(
						'id' => $row['u_id'],
						'checkBox' => '<input type="checkbox" name="users[]" value="' . $row['u_id'] . '" />',
						'group' => $s_group->post()->name($row['u_id'] . '-group'),
						'skupina' => $s_skupina->post()->name($row['u_id'] . '-skupina'),
						'dancer' => '<input type="checkbox" name="' . $row['u_id'] . '-dancer" value="dancer" />',
						'fullName' => $row['u_jmeno'] . ' ' . $row['u_prijmeni'],
						'narozeni' => formatDate($row['u_narozeni'])
				);
				$row = $new_data;
			}
			$this->render('files/View/Admin/Users/Unconfirmed.inc', array(
					'data' => $users
			));
			return;
		}
		if(post('action') == 'confirm') {
			foreach(post('users') as $id) {
				$data = DBUser::getUserData($id);
				
				DBUser::confirmUser($id, post($id . '-group'), post($id . '-skupina'),
					post($id . '-dancer') ? 1 : 0);
				Mailer::reg_confirmed_notice($data['u_email'], $data['u_login']);
			}
			$this->redirect('/admin/users', 'Uživatelé potvrzeni');
		} elseif(post('action') == 'remove') {
			$this->redirect('/admin/users/remove?' . http_build_query(array('u' => post('users'))));
		}
	}
	function duplicate($id = null) {
		if(!empty($_POST) && post('action') == 'remove' && post('users') && !empty($_POST['users']))
			$this->redirect('/admin/users/remove?' . http_build_query(array('u' => post('users'))));
		
		$users = DBUser::getDuplicateUsers();
		foreach($users as &$row) {
			$new_data = array(
					'id' => $row['u_id'],
					'checkBox' => '<input type="checkbox" name="users[]" value="' . $row['u_id'] . '" />',
					'colorBox' => getColorBox($row['s_color_text'], $row['s_description']),
					'fullName' => $row['u_prijmeni'] . ', ' . $row['u_jmeno'],
					'email' => $row['u_email'],
					'telefon' => $row['u_telefon'],
					'narozeni' => formatDate($row['u_narozeni']),
					'timestamp' => formatTimestamp($row['u_timestamp'])
			);
			$row = $new_data;
		}
		$this->render('files/View/Admin/Users/Duplicate.inc', array(
				'data' => $users
		));
	}
	function statistiky($id = null) {
		$all = DBUser::getUsers(L_ALL);
		$active = DBUser::getActiveUsers(L_ALL);
		$dancers = DBUser::getActiveDancers(L_ALL);
		$data = array(
			array('Uživatelé v databázi', count($all)),
			array('Aktivní uživatelé', count($active)),
			array('Aktivní tanečníci', count($dancers))
		);
		
		$groupcount = DBUser::getGroupCounts();
		foreach($groupcount as $group)
			$data[] = array($group['pe_name'], $group['count']);
		
		foreach($data as &$row) {
			$new_data = array(
					'group' => $row[0],
					'count' => $row[1]
			);
			$row = $new_data;
		}
		$this->render('files/View/Admin/Users/Statistiky.inc', array(
				'data' => $data
		));
	}
	function temporary($id = null) {
		$type = post('type');
		$jmeno = post('jmeno');
		$prijmeni = post('prijmeni');
		$narozeni = $this->date('narozeni')->getPost();
		
		$login = preg_replace('/[^a-zA-Z0-9.-_]*/','', strtolower($prijmeni)) . '_' .
				preg_replace('/[^a-zA-Z0-9.-_]*/','', strtolower($jmeno));
		
		if(!($id = DBUser::getUserByFullName($jmeno, $prijmeni)) && !($id = DBUser::getUserID($login))) {
			list($user_id, $par_id) = DBUser::addTemporaryUser($login, $jmeno, $prijmeni, $narozeni);
			
			header('Content-Type: application/json');
			echo json_encode(array('user_id' => $user_id, 'par_id' => $par_id, 'jmeno' => $jmeno,
				'prijmeni' => $prijmeni, 'narozeni' => (string) $narozeni, 'rok' => $narozeni->getYear()));
		} else {
			if(is_array($id))
				$id = $id['u_id'];
			
			$data = DBUser::getUserData($id);
			$partner = DBPary::getLatestPartner($data['u_id'], $data['u_pohlavi']);
			if($partner && $partner['p_id'])
				$par_id = $partner['p_id'];
			else
				$par_id = DBPary::noPartner($data['u_id']);
			
			$narozeni = explode('-', $data['u_narozeni']);
			
			header('Content-Type: application/json');
			echo json_encode(array('user_id' => $data['u_id'], 'par_id' => $par_id, 'jmeno' => $data['u_jmeno'],
				'prijmeni' => $data['u_prijmeni'], 'narozeni' => $data['u_narozeni'], 'rok' => array_shift($narozeni)));
		}
		exit;
	}
	
	private function displayOverview($action) {
		$groups = DBPermissions::getGroups();
		$group_lookup = array();
		foreach($groups as &$row) {
			if($row['pe_id'])
				$filter[] = $row['pe_id'];
			$new_data = array(
					'id' => $row['pe_id'],
					'name' => $row['pe_name']
			);
			$group_lookup[$row['pe_id']] = $row['pe_name'];
			$row = $new_data;
		}
		if($action == 'status') {
			$skupiny = DBSkupiny::get();
			$skupinyselect = $this->select()->post();
			foreach($skupiny as $skupina)
				$skupinyselect->option($skupina['s_id'], $skupina['s_name']);
		}
		$options['filter'] = in_array(get('f'), array_merge(array('dancer', 'system', 'all', 'unconfirmed', 'ban'), $filter)) ?
		get('f') : 'all';
		$pager = new Paging(new PagingAdapterDBSelect('DBUser', $options));
		$pager->setCurrentPageField('p');
		$pager->setItemsPerPageField('c');
		$pager->setDefaultItemsPerPage(20);
		$pager->setPageRange(5);
		$data = $pager->getItems();
		$i = $pager->getItemsPerPage() * ($pager->getCurrentPage() - 1);
		foreach($data as &$item) {
			$new_data = array(
					'checkBox' => '<input type="checkbox" name="users[]" value="' . $item['u_id'] . '" />',
					'index' => ++$i,
					'fullName' => $item['u_prijmeni'] . ', ' . $item['u_jmeno'],
					'colorBox' => getColorBox($item['s_color_text'], $item['s_description']),
					'groupInfo' => $group_lookup[$item['u_group']]
			);
			switch($action) {
				case 'platby':
					$new_data['varSymbol'] = User::var_symbol($item['u_id']);
					if($item['up_id'] && strcmp($item['up_plati_do'], date('Y-m-d')) >= 0) {
						$new_data['hasPaid'] = '<span style="color:#0B0;">' . $item['up_castka'] . ' Kč</span>';
						$new_data['dateUntil'] = formatDate($item['up_plati_do']);
					} else {
						$new_data['hasPaid'] = '<a href="/admin/platby/add?u=' . $item['u_id'] . '">NE</a>';
						if(!$item['up_id']) {
							$new_data['dateUntil'] = 'nikdy';
						} else {
							$now = time();
							$kdy = strtotime($item['up_plati_do']);
							
							$diff = $now - $kdy;
							if($diff > 31536000)
								$diffstr = 'více než rok';
							elseif($diff > 15768000)
							$diffstr = 'více než půl roku';
							elseif($diff > 7884000)
							$diffstr = 'více než 3 měsíce';
							elseif($diff > 5256000)
							$diffstr = 'více než 2 měsíce';
							elseif($diff > 2628000)
							$diffstr = 'více než měsíc';
							else
								$diffstr = 'méně než měsíc';
							$new_data['dateUntil'] = $diffstr;
						}
					}
					break;
				case 'status':
					$new_data['skupina'] = '<input type="hidden" name="save[]" value="' . $item['u_id'] . '"/>';
					$new_data['skupina'] .=  $skupinyselect->name($item['u_id'] . '-skupina')->value($item['u_skupina']);
					$new_data['dancer'] = '<label>' . getCheckbox($item['u_id'] . '-dancer', '1', $item['u_dancer']) . '</label>';
					$new_data['system'] = '<label>' . getCheckbox($item['u_id'] . '-system', '1', $item['u_system']) . '</label>';
					break;
				case 'info':
				default:
					$new_data['birthDate'] = formatDate($item['u_narozeni']);
					if($item['up_id'] && strcmp($item['up_plati_do'], date('Y-m-d')) >= 0)
						$new_data['hasPaid'] = '<span style="color:#0B0;">' . $item['up_castka'] . ' Kč</span>';
					else
						$new_data['hasPaid'] = '<a href="/admin/platby/add?u=' . $item['u_id'] . '">NE</a>';
					break;
			}
			$item = $new_data;
		}
		$this->render('files/View/Admin/Users/Overview.inc', array(
				'showMenu' => !TISK,
				'groups' => $groups,
				'data' => $data,
				'navigation' => $pager->getNavigation(),
				'view' => $action
		));
		return;
	}
	
	private function displayForm() {
		$groups = DBPermissions::getGroups();
		foreach($groups as &$item) {
			$new_data = array(
					'id' => $item['pe_id'],
					'name' => $item['pe_name']
			);
			$item = $new_data;
		}unset($item);
		$skupiny = DBSkupiny::get();
		foreach($skupiny as &$item) {
			$new_data = array(
					'id' => $item['s_id'],
					'color' => $item['s_color_text'],
					'popis' => $item['s_description']
			);
			$item = $new_data;
		}
		$this->render('files/View/Admin/Users/Form.inc', array(
				'action' => Request::getAction(),
				'groups' => $groups,
				'skupiny' => $skupiny
		));
	}
	
	private function checkData($data, $action = 'add') {
		$narozeni = $this->date('narozeni')->getPost();
		
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