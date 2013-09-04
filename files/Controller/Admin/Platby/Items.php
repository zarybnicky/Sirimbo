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
		
	}
	function edit($id = null) {
		
	}
	function remove($id = null) {
		if(!is_array(post('data')) && !is_array(get('u')))
			$this->redirect('/admin/platby/items');
		if(!empty($_POST) && post('action') == 'confirm') {
			foreach(post('data') as $id) {
				$item = DBPlatbyItem::getSingle($id);
				$itemRaw = DBPlatbyRaw::getSingle($item['pi_id_raw']);
				DBPlatbyItem::remove($id);
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
	private function getCategories() {
		$categories = DBPlatbyGroup::getGroupsWithCategories();
		$new = array();
		$group_id = 0;
		foreach($categories as $array) {
			if($group_id != $array['pg_id'])
				$new['group_' . $array['pg_id']] = "{$array['pg_name']}:";
			$new[(int) $array['pc_id']] = "{$array['pc_symbol']} - {$array['pc_name']}";
		}
		return $new;
	}
}