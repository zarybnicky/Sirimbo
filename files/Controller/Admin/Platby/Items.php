<?php
include_once('files/Controller/Admin/Platby.php');
class Controller_Admin_Platby_Items extends Controller_Admin_Platby {
	function __construct() {
		Permissions::checkError('platby', P_OWNED);
	}
	function view($id = null) {
		switch(post('action')) {
			case 'edit':
				$users = post('data');
				if($users[0])
					$this->redirect('/admin/platby/items/edit/' . $users[0]);
				break;
			case 'remove':
				if(!is_array(post('data')))
					break;
				$this->redirect('/admin/platby/items/remove?' . http_build_query(array('u' => post('data'))));
				break;
		}
		$data = $this->getData();

		$this->render('files/View/Admin/Platby/ItemsOverview.inc', array(
				'users' => DBUser::getUsers(),
				'categories' => $this->getCategories(),
				'data' => $data
		));
	}
	function add($id = null) {
		if(empty($_POST) || ($s = $this->checkPost()) != array()) {
			if(!empty($_POST))
				$this->redirect()->setRedirectMessage($s);
			$this->displayForm(0);
			return;
		}
		list($specific, $variable, $date, $amount) =
			$this->formatData(post('specific'), post('variable'), post('date'), post('amount'));
		
		DBPlatbyItem::insert($variable, $specific, '0', $amount, $date);
		$this->redirect('/admin/platby/items', 'Platba úspěšně přidána');
	}
	function edit($id = null) {
		if(!$id || !($data = DBPlatbyItem::getSingle($id)))
			$this->redirect('/admin/platby/items', 'Uživatel s takovým ID neexistuje');

		if(empty($_POST) || ($s = $this->checkPost()) != array()) {
			if(empty($_POST)) {
				post('date', $data['pi_date']);
				post('amount', $data['pi_amount']);
				post('variable', $data['pi_id_user']);
				post('specific', $data['pi_id_category']);
			} else {
				$this->redirect()->setRedirectMessage($s);
			}
			$this->displayForm($id);
			return;
		}
		list($specific, $variable, $date, $amount) =
			$this->formatData(post('specific'), post('variable'), post('date'), post('amount'));
		
		DBPlatbyItem::update($id, $variable, $specific, $amount, $date);
		$this->redirect('/admin/platby/items', 'Platba úspěšně upravena');
	}
	function remove($id = null) {
		if(!is_array(post('data')) && !is_array(get('u')))
			$this->redirect('/admin/platby/items');
		
		if(!empty($_POST) && post('action') == 'confirm') {
			foreach(post('data') as $id) {
				$item = DBPlatbyItem::getSingle($id);
				$itemRaw = DBPlatbyRaw::getSingle($item['pi_id_raw']);
				
				DBPlatbyItem::remove($id);
				if($item['pi_id_raw'])
					DBPlatbyRaw::update($item['pi_id_raw'], $itemRaw['pr_raw'], $itemRaw['pr_hash'], '0', '1');
			}
			$this->redirect('/admin/platby/items', 'Platby odebrány');
		}
		$data = array();
		foreach(get('u') as $id) {
			$item = DBPlatbyItem::getSingle($id, true);
			$data[] = array(
					'id' => $item['pi_id'],
					'text' => $item['u_jmeno'] . ' ' . $item['u_prijmeni'] . ' - ' . $item['pc_name']
			);
		}
		$this->render('files/View/Admin/RemovePrompt.inc', array(
				'header' => 'Správa plateb',
				'prompt' => 'Opravdu chcete odstranit platby:',
				'returnURL' => Request::getReferer(),
				'data' => $data
		));
	}
	private function displayForm($id) {
		$raw = array();
		if($id && ($item = DBPlatbyItem::getSingle($id)) && ($data = DBPlatbyRaw::getSingle($item['pi_id_raw']))) {
			$data = unserialize($data['pr_raw']);
			foreach($data as $key => $value) {
				$raw[] = array(
						'column' => $key,
						'value' => $value
				);
			}
		}
		$users = $this->getUsers();
		$categories = $this->getCategories();
		$this->render('files/View/Admin/Platby/ItemsForm.inc', array(
				'action' => Request::getAction(),
				'id' => $id,
				'raw' => $raw,
				'users' => $users,
				'categories' => $categories
		));
	}
	private function getCategories() {
		$out = $this->getCategoryLookup(false, false, true);
		foreach($out as $key => &$array) {
			if(strpos($key, 'group_') !== false)
				$array = "{$array['pg_name']}:";
			else
				$array = "{$array['pc_symbol']} - {$array['pc_name']}";
		}
		return $out;
	}
	private function getUsers() {
		$users = $this->getUserLookup(true);
		foreach($users as $key => &$array) {
			$array = User::var_symbol($array['u_id']) . " - {$array['u_prijmeni']}, {$array['u_jmeno']}";
		}
		return $users;
	}
	private function getData() {
		$filter = array();
		if(get('user') && is_numeric(get('user')))
			$filter['u_id'] = get('user');
		if(get('category')) {
			if(is_numeric(get('category'))) {
				$filter['pc_id'] = get('category');
			} else {
				$id = str_replace('group_', '', get('category'));
				if(is_numeric($id)) {
					$rows = DBPlatbyGroup::getSingleWithCategories($id);
					$ids = array();
					foreach($rows as $row)
						$ids[] = $row['pc_id'];
					$filter['pc_id'] = $ids;
				}
			}
		}
		$data = DBPlatbyItem::get(true, $filter);
		foreach($data as &$row) {
			$new_data = array(
					'checkBox' => '<input type="checkbox" name="data[]" value="' . $row['pi_id'] . '" />',
					'fullName' => $row['u_prijmeni'] . ', ' . $row['u_jmeno'],
					'category' => $row['pc_name'],
					'date' => (new Date($row['pi_date']))->getDate(Date::FORMAT_SIMPLE_SPACED),
					'amount' => $row['pi_amount'] . 'Kč'
			);
			$row = $new_data;
		}
		return $data;
	}
}