<?php
include_once('files/Controller/Admin.php');
class Controller_Admin_Platby extends Controller_Admin {
	function __construct() {
		Permissions::checkError('platby', P_OWNED);
	}
	function view($id = null) {
		switch(post('action')) {
			case 'edit':
				$platby = post('platby');
				if($platby[0])
					$this->redirect('/admin/platby/edit/' . $platby[0]);
				break;
			case 'remove':
				if(!is_array(post('platby')))
					break;
				$this->redirect('/admin/platby/remove?' . http_build_query(array('u' => post('platby'))));
				break;
		}
		if(!get('f'))
			get('f', 'all');
		
		if(get('f') != 'all') {
			list($year, $n) = explode('-', get('f'));
			
			if($n == 1) {
				$od = $year . Settings::$platby_obdobi['1-pololeti'][0];
				$do = ((int) $year + 1) . Settings::$platby_obdobi['1-pololeti'][1];
			} elseif($n == 2) {
				$od = ((int) $year + 1) . Settings::$platby_obdobi['2-pololeti'][0];
				$do = ((int) $year + 1) . Settings::$platby_obdobi['2-pololeti'][1];
			}
			$options = array('type' => 'date', 'od' => $od, 'do' => $do);
		} else {
			$options['type'] = 'platby';
		}
		$options['sort'] = in_array(get('s'), array('prijmeni', 'placeno', 'var-symbol')) ? get('s') : 'prijmeni';
		$pager = new Paging(new PagingAdapterDBSelect('DBPlatby', $options));
		$pager->setCurrentPageField('p');
		$pager->setItemsPerPageField('c');
		$pager->setDefaultItemsPerPage(20);
		$pager->setPageRange(5);
		$data = $pager->getItems();
		foreach($data as &$row) {
			$new_data = array(
					'checkBox' => '<input type="checkbox" name="platby[]" value="' . $row['up_id'] . '" />',
					'fullName' => $row['u_prijmeni'] . ', ' . $row['u_jmeno'],
					'colorBox' => getColorBox($row['us_color'], $row['us_popis']),
					'varSymbol' => User::var_symbol($row['u_id']),
					'obdobi' => $row['up_obdobi'] == true ? Settings::$platby_obdobi[$row['up_obdobi']][3] : '',
					'paidDate' => formatDate($row['up_placeno']),
					'paidUntil' => formatDate($row['up_plati_do'])
			);
			if((strpos($row['up_obdobi'], 'pololeti') && $row['up_castka'] != $row['us_platba_pulrok']) ||
				(strpos($row['up_obdobi'], 'ctvrtleti') && $row['up_castka'] != $row['us_platba_ctvrtrok']))
				$new_data['castka'] = '<span style="color:black;font-weight:bold;">' .$row['up_castka'] . ' Kč</span>';
			else
				$new_data['castka'] = '<span style="color:#0B0;">' . $row['up_castka'] . ' Kč</span>';
			$row = $new_data;
		}
		$totalPaid = $pager->getPages();
		$totalPaid = $totalPaid['total'];
		$this->render('files/View/Admin/Platby/Overview.inc', array(
				'showMenu' => !TISK,
				'navigation' => $pager->getNavigation(),
				'data' => $data,
				'totalUsers' => count(DBUser::getUsers()),
				'totalPaid' => $totalPaid,
				'filter' => get('f')
		));
	}
	function add($id = null) {
		if(empty($_POST) || is_object($f = $this->checkData($_POST, 'add'))) {
			if(!empty($_POST)) {
				$this->redirect()->setRedirectMessage($f->getMessages());
			}
			post('user', get('u'));
			$this->displayForm();
			return;
		}
		$placeno = $this->date('placeno')->getPost();
		$obdobi = post('obdobi');
		list($plati_od, $plati_do) = $this->processPlatbaData($placeno, $obdobi);
/*
		$platby = DBPlatby::getPlatbyFromUser(post('user'));
		foreach($platby as $platba) {
			if($platba['up_obdobi'] != $obdobi)
				continue;
			
			DBPlatby::editPlatba($platba['up_id'], post('user'), $obdobi,
				post('castka') + $platba['up_castka'], $placeno, $plati_od,
				$plati_do);
			
			$this->redirect(post('referer') ? post('referer') : '/admin/platby',
				'Druhá platba za stejné období - sloučeno!');
		}
*/
		DBPlatby::addPlatba(post('user'), $obdobi, post('castka'), $placeno, $plati_od, $plati_do);
		
		$this->redirect(post('referer') ? post('referer') : '/admin/platby',
			'Platba úspěšně přidána');
	}
	function edit($id = null) {
		if(!$id || !($data = DBPlatby::getSinglePlatba($id)))
			$this->redirect(post('referer') ? post('referer') : '/admin/platby',
				'Platba s takovým ID neexistuje');
		if(empty($_POST) || is_object($f = $this->checkData($_POST, 'edit'))) {
			if(empty($_POST)) {
				post('user', $data['up_id_user']);
				post('castka', $data['up_castka']);
				post('obdobi', $data['up_obdobi']);
				post('placeno', $data['up_placeno']);
			} else {
				$this->redirect()->setRedirectMessage($f->getMessages());
			}
			$this->displayForm();
			return;
		}
		$placeno = $this->date('placeno')->getPost();
		$obdobi = post('obdobi');
		list($plati_od, $plati_do) = $this->processPlatbaData($placeno, $obdobi);
/*
		$platby = DBPlatby::getPlatbyFromUser(post('user'));
		foreach($platby as $platba) {
			if($platba['up_obdobi'] != $obdobi || $platba['up_id'] == $id)
				continue;
			
			DBPlatby::editPlatba($platba['up_id'], post('user'), $obdobi,
				post('castka') + $platba['up_castka'], $placeno, $plati_od, $plati_do);
			DBPlatby::removePlatba($id);
			
			$this->redirect(post('referer') ? post('referer') : '/admin/platby',
				'Druhá platba za stejné období - sloučeno!');
		}
*/
		DBPlatby::editPlatba($id, post('user'), $obdobi, post('castka'),
			$placeno, $plati_od, $plati_do);
		
		$this->redirect(post('referer') ? post('referer') : '/admin/platby',
			'Platba úspěšně upravena');
	}
	function remove($id = null) {
		if(!is_array(post('data')) && !is_array(get('u')))
			$this->redirect('/admin/platby');
		if(!empty($_POST) && post('action') == 'confirm') {
			foreach(post('data') as $id)
				DBPlatby::removePlatba($id);
			$this->redirect('/admin/platby', 'Platby odebrány');
		}
		$data = array();
		foreach(get('u') as $id) {
			$item = DBPlatby::getSinglePlatba($id);
			$data[] = array(
					'id' => $id,
					'text' => 'od "' . $item['u_prijmeni'] . ', ' . $item['u_jmeno'] .
						'" - placeno ' . formatDate($item['up_placeno'])
			);
		}
		$this->render('files/View/Admin/RemovePrompt.inc', array(
				'header' => 'Správa plateb',
				'prompt' => 'Opravdu chcete odstranit platby:',
				'returnURL' => Request::getReferer(),
				'data' => $data
		));
	}
	
	private function displayForm() {
		$aktualni = post('user');
		if($aktualni && is_numeric($aktualni) && ($user = DBUser::getUserData($aktualni))) {
			$currentUser = array(
					'fullName' => $user['u_prijmeni'] . ', ' . $user['u_jmeno'],
					'colorBox' => getColorBox($user['us_color'], $user['us_popis'])
			);
		} else {
			$currentUser = array();
		}
		$skupiny = DBSkupiny::getSkupiny();
		foreach($skupiny as &$row) {
			$new_data = array(
					'description' => getColorBox($row['us_color'], $row['us_popis']) . '&nbsp;' . $row['us_popis'],
					'platbaMesic' => $row['us_platba_mesic'],
					'platbaCtvrtleti' => $row['us_platba_ctvrtrok'],
					'platbaPololeti' => $row['us_platba_pulrok']
			);
			$row = $new_data;
		}
		$this->render('files/View/Admin/Platby/Form.inc', array(
				'action' => Request::getAction(),
				'returnURL' => Request::getReferer(),
				'currentUser' => $currentUser,
				'skupiny' => $skupiny,
				'users' => DBUser::getUsers()
		));
	}
	
	private function processPlatbaData($placeno, $obdobi) {
		list($year, $month, $day) = explode('-', $placeno);
		if(strcmp($placeno, $year . '-06-30') <= 0)
			$plati_od = ($year - 1) . Settings::$platby_obdobi[$obdobi][0];
		else
			$plati_od = $year . Settings::$platby_obdobi[$obdobi][0];
		
		if(strcmp($placeno, $year . '-06-30') <= 0 &&
				$obdobi == '1-ctvrtleti') {
			$year = (string) (((int) $year) - 1);
		} elseif(strcmp($placeno, $year . '-07-01') >= 0 &&
				($obdobi == '1-pololeti' || $obdobi == '2-pololeti' ||
						$obdobi == '2-ctvrtleti' || $obdobi == '3-ctvrtleti' ||
						$obdobi == '4-ctvrtleti')) {
			$year = (string) (((int) $year) + 1);
		}
		$plati_do = $year . Settings::$platby_obdobi[$obdobi][1];
		return array($plati_od, $plati_do);
	}
	
	private function checkData($data, $action = 'add') {
		$placeno = $this->date('placeno')->getPost();
		
		$f = new Form();
		$f->checkNumeric(post('user'), 'Neplatný uživatel', 'user');
		$f->checkNumeric(post('castka'), 'Částka musí být zadaná jen čísly', 'castka');
		$f->checkDate($placeno, 'Neplatný formát data', 'placeno');

		return $f->isValid() ? true : $f;
	}
}
?>