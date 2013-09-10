<?php
include_once('files/Controller/Admin/Platby.php');
class Controller_Admin_Platby_Category extends Controller_Admin_Platby {
	function __construct() {
		Permissions::checkError('platby', P_OWNED);
	}
	function view($id = null) {
		$this->render('files/View/Admin/Platby/CategoryOverview.inc', array(
				'data' => $this->getCategories(),
				'orphanGroup' => $this->getOrphanGroup(),
				'orphanCategory' => $this->getOrphanCategory()
		));
	}
	function add_group($id = null) {
		if(empty($_POST) || is_object($s = $this->checkGroupPost())) {
			if(empty($_POST)) {
				post('base', 1);
			} else {
				$this->redirect()->setRedirectMessage($s->getMessages());
			}
			$this->displayGroupForm('add');
			return;
		}
		DBPlatbyGroup::insert(post('type'), post('name'), post('description'), post('base'));
		$this->redirect('/admin/platby/category', 'Kategorie úspěšně přidána');
	}
	function edit_group($id = null) {
		if(!$id || !($data = DBPlatbyGroup::getSingle($id)))
			$this->redirect('/admin/platby/category', 'Kategorie s takovým ID neexistuje');
		
		if(empty($_POST) || is_object($s = $this->checkGroupPost())) {
			if(empty($_POST)) {
				post('type', $data['pg_type']);
				post('name', $data['pg_name']);
				post('description', $data['pg_description']);
				post('base', $data['pg_base']);
			} else {
				$this->redirect()->setRedirectMessage($s->getMessages());
			}
			$this->displayGroupForm('edit', DBPlatbyGroup::getSingleWithCategories($id));
			return;
		}
		
		//TODO: post(skupiny)[] !!!
		
		DBPlatbyGroup::update($id, post('type'), post('name'), post('description'), post('base'));
		$this->redirect('/admin/platby/category', 'Kategorie úspěšně upravena');
	}
	function remove_group($id = null) {
		if(!$id || !($data = DBPlatbyGroup::getSingle($id)))
			$this->redirect('/admin/platby/category', 'Kategorie s takovým ID neexistuje');
		
		if(post('action') == 'unlink') {
			$f = $this->getLinkedGroupObjects($id);
			
			$categoryCount = 0;
			foreach($f['categories'] as $data) {
				//remove connection c<->g
			}unset($data);
			$skupinaCount = 0;
			foreach($f['skupiny'] as $data) {
				//remove connection g<->s
			}
			$this->redirect('/admin/platby/category/remove_group/' . $id,
					'Spojení s "' . $skupinaCount . '" skupinami a s "' . $categoryCount . '" kategoriemi bylo odstraněno');
			return;
		}
		if(((empty($_POST) || post('action') == 'confirm') && ($f = $this->getLinkedGroupObjects($id))) || empty($_POST)) {
			if(isset($f) && $f) {
				$this->redirect()->setRedirectMessage(
						'Nemůžu odstranit kategorii s připojenými skupinami nebo specifickými symboly! ' . 
						'<form action="" method="post"><button type="submit" name="action" value="unlink">Odstranit spojení?</button></form>'
				);
			}
			$this->render('files/View/Admin/Platby/CategoryGroupRemove.inc', array(
					'id' => $id,
					'name' => $data['pg_name']
			));
			return;
		}
		//DB...
		$this->redirect('/admin/platby/category', 'Kategorie byla odebrána');
	}
	function add_category($id = null) {
		
	}
	function edit_category($id = null) {
		
	}
	function remove_category($id = null) {
		
	}
	private function getLinkedGroupObjects($id) {
		$cat = DBPlatbyGroup::getSingleWithCategories($id);
		$sku = DBPlatbyGroup::getSingleWithSkupiny($id);
		
		if(empty($cat) && empty($sku))
			return array();
		else
			return array('categories' => $cat, 'skupiny' => $sku);
	}
	private function displayGroupForm($action, $data = array()) {
		$id = Request::getID() ? Request::getID() : 0;
		foreach($data as $key => &$array) {
			$new_data = array(
					'buttons' => '<a href="/admin/platby/category/edit_category/' . $array['pc_id'] . '">Upravit</a>',
					'name' => $array['pc_name'],
					'specific' => $array['pc_symbol'],
					'amount' => $array['pc_amount'],
					'dueDate' => (new Date($array['pc_date_due']))->getDate(Date::FORMAT_SIMPLE_SPACED),
					'validDate' => $this->getDateDisplay($array['pc_valid_from'], $array['pc_valid_to']),
					'usePrefix' => '&nbsp;' . ($array['pc_use_prefix'] ? '&#10003;' : '&#10799;'),
					'useBase' => '&nbsp;' . ($array['pc_use_base'] ? '&#10003;' : '&#10799;'),
					'archive' => '&nbsp;' . ($array['pc_archive'] ? '&#10003;' : '&#10799;')
			);
			$array = $new_data;
		}unset($array);
		$skupiny = DBSkupiny::getSkupiny();
		$skupinySelect = '';
		foreach($skupiny as &$item) {
			$skupinySelect .= '<label>' .
					'<input type="checkbox" name="skupiny[]" value="' . $item['us_id'] . '"' .
						(post('skupiny') ? (array_search($item['us_id'], post('skupiny')) !== false ? ' checked="checked"' : '') : '') . '/>' .
					getColorBox($item['us_color'], $item['us_popis']) . '&nbsp;' . $item['us_popis'] .
					'</label><br/>';
		}
		$symbols = DBPlatbyCategory::getNotInGroup($id);
		$symbolSelect = array();
		foreach($symbols as $array) {
			$symbolSelect[$array['pc_id']] = $array['pc_symbol'] . ' - ' . $array['pc_name'];
		}
		$this->render('files/View/Admin/Platby/CategoryGroupForm.inc', array(
				'id' => $id,
				'action' => $action,
				'categories' => $data,
				'skupinySelect' => $skupinySelect,
				'symbols' => $symbolSelect
		));
	}
	private function getCategories() {
		$categories = parent::getCategoryLookup(false, false, true);
		foreach($categories as $key => &$array) {
			$new_data = array();
			if(strpos($key, 'group_') !== false) {
				$new_data['name'] = '<span class="big">' . $array['pg_name'] . '</span>';
				$new_data['colorBox'] = 'TODO: colorBox';
				$new_data['validDate'] = '';
				$new_data['buttons'] = $this->getEditLink('/admin/platby/category/edit_group/' . $array['pg_id']) .
						$this->getRemoveLink('/admin/platby/category/remove_group/' . $array['pg_id']);
			} else {
				$new_data['name'] = '(' . $array['pc_symbol'] . ') - ' . $array['pc_name'];
				$new_data['colorBox'] = '';
				$new_data['validDate'] = $this->getDateDisplay($array['pc_valid_from'], $array['pc_valid_to']);
				$new_data['buttons'] = $this->getEditLink('/admin/platby/category/edit_category/' . $array['pc_id']) . 
					$this->getRemoveLink('/admin/platby/category/remove_category/' . $array['pc_id']);
			}
			$array = $new_data;
		}
		return $categories;
	}
	private function getOrphanGroup() {
		$out = DBPlatbyGroup::getOrphan();
		foreach($out as &$array) {
			$new_data = array(
					'name' => $array['pg_name'],
					'buttons' => $this->getEditLink('/admin/platby/category/edit_group/' . $array['pg_id']) .
						$this->getRemoveLink('/admin/platby/category/remove_group/' . $array['pg_id'])
			);
			$array = $new_data;
		}
		return $out;
	}
	private function getOrphanCategory() {
		$out = DBPlatbyCategory::getOrphan();
		foreach($out as &$array) {
			$new_data = array(
					'name' => $array['pc_name'],
					'buttons' => $this->getEditLink('/admin/platby/category/edit_category/' . $array['pc_id']) . 
						$this->getRemoveLink('/admin/platby/category/remove_category/' . $array['pc_id'])
			);
			$array = $new_data;
		}
		return $out;
	}
	private function getEditLink($link) {
		return '<a href="' . $link . '"><img alt="Upravit" src="/images/wrench.png" /></a>';
	}
	private function getRemoveLink($link) {
		return '<a href="' . $link . '"><img alt="Odstranit" src="/images/cross.png" /></a>';
	}
	private function checkGroupPost() {
		$f = new Form();
		$f->checkInArray(post('type'), array('0', '1'), 'Neplatný typ kategorie');
		$f->checkNotEmpty(post('name'), 'Zadejte nějaký název platby');
		$f->checkNumeric(post('base'), 'Násobitel musí být zadán pouze čisly');
		
		return $f->isValid() ? true : $f;
	}
	private function getDateDisplay($from, $to) {
		$out = (new Date($from))->getDate(Date::FORMAT_SIMPLE_SPACED);
		$to = new Date($to);
		if((int) $to->getMonth())
			$out .= ' - ' . $to->getDate(Date::FORMAT_SIMPLE_SPACED);
		return $out;
	}
}