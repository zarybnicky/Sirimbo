<?php
include_once('files/Controller/Admin/Platby.php');
class Controller_Admin_Platby_Category extends Controller_Admin_Platby {
	function __construct() {
		Permissions::checkError('platby', P_OWNED);
	}
	function view($id = null) {
		$this->render('files/View/Admin/Platby/CategoryOverview.inc', array(
				'data' => $this->getCategories(),
				'orphanGroupSkupina' => $this->getOrphanGroupSkupina(),
				'orphanGroupCategory' => $this->getOrphanGroupCategory(),
				'orphanCategory' => $this->getOrphanCategory()
		));
	}
	function add_group($id = null) {
		if(empty($_POST) || is_object($s = $this->checkGroupPost())) {
			if(empty($_POST)) {
				post('base', 1);
			} else {
				$this->redirect()->setMessage($s->getMessages());
			}
			$this->displayGroupForm('add');
			return;
		}
		DBPlatbyGroup::insert(post('type'), post('name'), post('description'), post('base'));
		$insertId = DBPlatbyGroup::getInsertId();
		if(get('category') && ($data = DBPlatbyCategory::getSingle(get('category')))) {
			DBPlatbyGroup::addChild($insertId, get('category'));
			$skupiny = DBPlatbyGroup::getSingleWithSkupiny($insertId);
			$conflicts = array();
			foreach($skupiny as $array)
				$conflicts = array_merge($conflicts, DBPlatby::checkConflicts($array['s_id']));
				
			if(!empty($conflicts)) {
				DBPlatbyGroup::removeChild($insertId, get('category'));
				$this->redirect('/admin/platby/category/edit_category/' . get('category'),
						'Kategorie byla přidána, ale nebyla přiřazena - takové přiřazení není platné.');
			}
			$this->redirect('/admin/platby/category/edit_category/' . get('category'), 'Kategorie úspěšně přidána a přiřazena');
		} elseif(get('skupina') && ($data = DBSkupiny::getSingle(get('skupina')))) {
			DBSkupiny::addChild(get('skupina'), $insertId);
			$conflicts = DBPlatby::checkConflicts(get('skupina'));
			if(!empty($conflicts)) {
				DBSkupiny::removeChild(get('skupina'), $insertId);
				$this->redirect('/admin/skupiny/edit/' . get('skupina'),
						'Kategorie byla přidána, ale nebyla přiřazena - takové přiřazení není platné.');
			}
			$this->redirect('/admin/skupiny/edit/' . get('skupiny'), 'Kategorie úspěšně přidána a přiřazena');
		}
		$this->redirect('/admin/platby/category', 'Kategorie úspěšně přidána');
	}
	function edit_group($id = null) {
		if(!$id || !($data = DBPlatbyGroup::getSingle($id)))
			$this->redirect('/admin/platby/category', 'Kategorie s takovým ID neexistuje');
		
		if(post('action') == 'skupiny') {
			if(!($data = DBSkupiny::getSingle(post('skupiny'))))
				$this->redirect('/admin/platby/category/edit_group/' . $id, 'Kategorie s takovým ID neexistuje.');
			
			DBSkupiny::addChild(post('skupiny'), $id);
			$conflicts = DBPlatby::checkConflicts(post('skupiny'));
			
			if(!empty($conflicts)) {
				DBSkupiny::removeChild(post('skupiny'), $id);
				$this->redirect('/admin/platby/category/edit_group/' . $id,
					'Takové přiřazení není platné - způsobilo by, že jeden specifický symbol by byl v jedné skupině dvakrát.');
			}
			$this->redirect('/admin/platby/category/edit_group/' . $id, 'Kategorie byla úspěšně přiřazena.');
		} elseif(post('action') == 'skupina_remove') {
			if(!($data = DBSkupiny::getSingle(post('skupina'))))
				$this->redirect('/admin/platby/category/edit_group/' . $id, 'Skupina s takovým ID neexistuje.');
			
			DBSkupiny::removeChild(post('skupina'), $id);
			$this->redirect('/admin/platby/category/edit_group/' . $id, 'Spojení s kategorií bylo úspěšně odstraněno.');
		} elseif(post('action') == 'category') {
			if(!($data = DBPlatbyCategory::getSingle(post('category'))))
				$this->redirect('/admin/platby/category/edit_group/' . $id, 'Kategorie s takovým ID neexistuje.');
			
			DBPlatbyGroup::addChild($id, post('category'));
			$skupiny = DBPlatbyGroup::getSingleWithSkupiny($id);
			$conflicts = array();
			foreach($skupiny as $array)
				$conflicts = array_merge($conflicts, DBPlatby::checkConflicts($array['s_id']));
			
			if(!empty($conflicts)) {
				DBPlatbyGroup::removeChild($id, post('category'));
				$this->redirect('/admin/platby/category/edit_group/' . $id,
					'Takové přiřazení není platné - způsobilo by, že jeden specifický symbol by byl v jedné skupině dvakrát.');
			}
			$this->redirect('/admin/platby/category/edit_group/' . $id, 'Kategorie byla úspěšně přiřazena.');
		} elseif(post('action') == 'category_remove') {
			if(!($data = DBPlatbyCategory::getSingle(post('category'))))
				$this->redirect('/admin/platby/category/edit_group/' . $id, 'Specifický symbol s takovým ID neexistuje.');
			
			DBPlatbyGroup::removeChild($id, post('category'));
			$this->redirect('/admin/platby/category/edit_group/' . $id, 'Spojení se specifickým symbolem bylo úspěšně odstraněno.');
		}
		
		if(empty($_POST) || is_object($s = $this->checkGroupPost())) {
			if(empty($_POST)) {
				post('type', $data['pg_type']);
				post('name', $data['pg_name']);
				post('description', $data['pg_description']);
				post('base', $data['pg_base']);
			} else {
				$this->redirect()->setMessage($s->getMessages());
			}
			$this->displayGroupForm('edit', DBPlatbyGroup::getSingleWithCategories($id));
			return;
		}
		
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
				DBPlatbyGroup::removeChild($id, $data['pc_id']);
				++$categoryCount;
			}
			unset($data);
			$skupinaCount = 0;
			foreach($f['skupiny'] as $data) {
				DBSkupiny::removeChild($data['s_id'], $id);
				++$skupinaCount;
			}
			$this->redirect('/admin/platby/category/remove_group/' . $id,
					'Spojení s \'' . $skupinaCount . '\' skupinami a s \'' . $categoryCount . '\' kategoriemi bylo odstraněno');
			return;
		}
		if(((empty($_POST) || post('action') == 'confirm') && ($f = $this->getLinkedGroupObjects($id))) || empty($_POST)) {
			if(isset($f) && $f) {
				$this->redirect()->setMessage(
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
		DBPlatbyGroup::delete($id);
		$this->redirect('/admin/platby/category', 'Kategorie byla odebrána');
	}
	function add_category($id = null) {
		if(empty($_POST) || is_object($s = $this->checkCategoryPost())) {
			if(!empty($_POST)) {
				$this->redirect()->setMessage($s->getMessages());
			}
			//TODO: default specific symbol! (next in line)
			$this->displayCategoryForm('add');
			return;
		}
		$dueDate = $this->date('dueDate')->getPost();
		
		$validRange = $this->date('validRange')->range()->getPostRange();
		$validFrom = $validRange['from'];
		$validTo = $validRange['to'];
		if(!$validTo->isValid())
			$validTo = $validFrom;
		elseif(strcasecmp((string) $validFrom, (string) $validTo) > 0)
			$validFrom = $validTo;
		
		$amount = post('amount');
		$use_base = '0';
		if(strpos($amount, '*')) {
			$use_base = '1';
			$amount = str_replace('*', '', $amount);
		}
		$use_prefix = post('usePrefix') ? '1' : '0';
		$archive = post('archive') ? '1' : '0';
		
		DBPlatbyCategory::insert(post('name'), post('symbol'), post('amount'), (string) $dueDate,
			(string) $validFrom, (string) $validTo, $use_base, $use_prefix, $archive);
		$insertId = DBPlatbyCategory::getInsertId();
		if(get('group') && ($data = DBPlatbyGroup::getSingle(get('group')))) {
			DBPlatbyGroup::addChild(get('group'), $insertId);
			$skupiny = DBPlatbyGroup::getSingleWithSkupiny(get('group'));
			$conflicts = array();
			foreach($skupiny as $array)
				$conflicts = array_merge($conflicts, DBPlatby::checkConflicts($array['s_id']));
				
			if(!empty($conflicts)) {
				DBPlatbyGroup::removeChild(get('category'), $insertId);
				$this->redirect('/admin/platby/category/edit_group/' . get('group'),
						'Specifický symbol byl přidán, ale nebyl přiřazen - takové přiřazení není platné.');
			}
			$this->redirect('/admin/platby/category/edit_group/' . get('group'), 'Specifický symbol úspěšně přidán a přiřazen');
		}
		$this->redirect('/admin/platby/category', 'Specifický symbol úspěšně přidán');
	}
	function edit_category($id = null) {
		if(!$id || !($data = DBPlatbyCategory::getSingle($id)))
			$this->redirect('/admin/platby/category', 'Kategorie s takovým ID neexistuje');
		
		if(post('action') == 'group') {
			if(!($data = DBPlatbyGroup::getSingle(post('group'))))
				$this->redirect('/admin/platby/category/edit_category/' . $id, 'Kategorie s takovým ID neexistuje.');
			
			DBPlatbyGroup::addChild(post('group'), $id);
			$skupiny = DBPlatbyGroup::getSingleWithSkupiny(post('group'));
			$conflicts = array();
			foreach($skupiny as $array)
				$conflicts = array_merge($conflicts, DBPlatby::checkConflicts($array['s_id']));
			
			if(!empty($conflicts)) {
				DBPlatbyGroup::removeChild(post('group'), $id);
				$this->redirect('/admin/platby/category/edit_category/' . $id,
					'Takové přiřazení není platné - způsobilo by, že jeden specifický symbol by byl v jedné skupině dvakrát.');
			}
			$this->redirect('/admin/platby/category/edit_category/' . $id, 'Kategorie byla úspěšně přiřazena.');
		} elseif(post('action') == 'group_remove') {
			if(!($data = DBPlatbyGroup::getSingle(post('group'))))
				$this->redirect('/admin/platby/category/edit_category/' . $id, 'Kategorie s takovým ID neexistuje.');
			
			DBPlatbyGroup::removeChild(post('group'), $id);
			$this->redirect('/admin/platby/category/edit_category/' . $id, 'Spojení s kategorií bylo úspěšně odstraněno.');
		}
		
		if(empty($_POST) || is_object($s = $this->checkCategoryPost())) {
			if(!empty($_POST)) {
				$this->redirect()->setMessage($s->getMessages());
			} else {
				if($data['pc_use_base']) {
					$data['pc_amount'] = '*' . $data['pc_amount'];
				}
				post('name', $data['pc_name']);
				post('symbol', $data['pc_symbol']);
				post('amount', $data['pc_amount']);
				post('dueDate', $data['pc_date_due']);
				post('validRange', $data['pc_valid_from'] . ' - ' . $data['pc_valid_to']);
				post('usePrefix', $data['pc_use_prefix']);
				post('archive', $data['pc_archive']);	
			}
			$this->displayCategoryForm('edit');
			return;
		}
		$dueDate = $this->date('dueDate')->getPost();
		
		$validRange = $this->date('validRange')->range()->getPostRange();
		$validFrom = $validRange['from'];
		$validTo = $validRange['to'];
		if(!$validTo->isValid())
			$validTo = $validFrom;
		elseif(strcasecmp((string) $validFrom, (string) $validTo) > 0)
			$validFrom = $validTo;
		
		$amount = post('amount');
		$use_base = '0';
		if(strpos($amount, '*')) {
			$use_base = '1';
			$amount = str_replace('*', '', $amount);
		}
		$use_prefix = post('usePrefix') ? '1' : '0';
		$archive = post('archive') ? '1' : '0';

		DBPlatbyCategory::update($id, post('name'), post('symbol'), post('amount'), (string) $dueDate,
			(string) $validFrom, (string) $validTo, $use_base, $use_prefix, $archive);
		if(get('group'))
			$this->redirect('/admin/platby/category/edit_group/' . get('group'), 'Specifický symbol úspěšně upraven');
		$this->redirect('/admin/platby/category', 'Specifický symbol úspěšně upraven');
	}
	function remove_category($id = null) {
		if(!$id || !($data = DBPlatbyCategory::getSingle($id)))
			$this->redirect('/admin/platby/category', 'Specifický symbol s takovým ID neexistuje');
		
		if(post('action') == 'unlink') {
			$f = $this->getLinkedCategoryObjects($id);
			
			$groupCount = 0;
			foreach($f['groups'] as $data) {
				DBPlatbyGroup::removeChild($data['pg_id'], $id);
				++$groupCount;
			}
			unset($data);
			$itemCount = 0;
			foreach($f['items'] as $data) {
				$raw = DBPlatbyRaw::getSingle($data['pi_id_raw']);
				DBPlatbyRaw::update($raw['pr_id'], $raw['pr_raw'], $raw['pr_hash'], '0', '0');
				DBPlatbyItem::remove($data['pi_id']);
				++$itemCount;
			}
			$this->redirect('/admin/platby/category/remove_category/' . $id,
					'Spojení s \'' . $groupCount . '\' kategoriemi a s \'' . $itemCount . '\' platbami bylo odstraněno');
			return;
		}
		if(((empty($_POST) || post('action') == 'confirm') && ($f = $this->getLinkedCategoryObjects($id))) || empty($_POST)) {
			if(isset($f) && $f) {
				$this->redirect()->setMessage(
						'Nemůžu odstranit specifický symbol s připojenými kategoriemi nebo položkami! ' . 
						'<form action="" method="post">' .
							'<button type="submit" name="action" value="unlink">' .
							'Odstranit spojení a přesunout ovlivněné platby do nezařazených?</button>' .
						'</form>'
				);
			}
			$this->render('files/View/Admin/Platby/CategorySymbolRemove.inc', array(
					'id' => $id,
					'name' => $data['pc_name']
			));
			return;
		}
		DBPlatbyCategory::delete($id);
		$this->redirect('/admin/platby/category', 'Specifický symbol byl odebrán');
	}
	private function getLinkedGroupObjects($id) {
		$cat = DBPlatbyGroup::getSingleWithCategories($id);
		$sku = DBPlatbyGroup::getSingleWithSkupiny($id);
		
		if(empty($cat) && empty($sku))
			return array();
		else
			return array('categories' => $cat, 'skupiny' => $sku);
	}
	private function getLinkedCategoryObjects($id) {
		$group = DBPlatbyCategory::getSingleWithGroups($id);
		$items = DBPlatbyItem::get(true, array('pc_id' => $id));
		
		if(empty($group) && empty($items))
			return array();
		else
			return array('groups' => $group, 'items' => $items);
	}
	private function displayCategoryForm($action) {
		$id = Request::getID() ? Request::getID() : 0;
		
		$groups = DBPlatbyCategory::getSingleWithGroups($id);
		foreach($groups as &$array) {
			$new_data = array(
					'buttons' => '<form action="" method="post">' .
						$this->getUnlinkGroupButton($array['pg_id']) .
						$this->getEditLink('/admin/platby/category/edit_group/' . $array['pg_id']) .
						$this->getRemoveLink('/admin/platby/category/remove_group/' . $array['pg_id']) .
						'</form>',
					'type' => ($array['pg_type'] == '1' ? 'Členské příspěvky' : 'Běžné platby'),
					'name' => $array['pg_name'],
					'base' => $array['pg_base']
			);
			$array = $new_data;
		}unset($array);

		$groupNotInCategory = DBPlatbyGroup::getNotInCategory($id);
		$groupSelect = array();
		foreach($groupNotInCategory as $array) {
			$groupSelect[$array['pg_id']] = $array['pg_name'];
		}unset($array);
		
		$this->render('files/View/Admin/Platby/CategorySymbolForm.inc', array(
				'id' => $id,
				'action' => $action,
				'groups' => $groups,
				'groupSelect' => $groupSelect
		));
	}
	private function displayGroupForm($action, $data = array()) {
		$id = Request::getID() ? Request::getID() : 0;
		foreach($data as $key => &$array) {
			$new_data = array(
					'buttons' => '<form action="" method="post">' .
						$this->getUnlinkCategoryButton($array['pc_id']) .
						$this->getEditLink('/admin/platby/category/edit_category/' . $array['pc_id']) .
						$this->getRemoveLink('/admin/platby/category/remove_category/' . $array['pc_id']) .
						'</form>',
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
		
		$categoryNotInGroup = DBPlatbyCategory::getNotInGroup($id);
		$categorySelect = array();
		foreach($categoryNotInGroup as $array) {
			$categorySelect[$array['pc_id']] = $array['pc_symbol'] . ' - ' . $array['pc_name'];
		}unset($array);
		
		$skupiny = DBPlatbyGroup::getSingleWithSkupiny($id);
		foreach($skupiny as &$array) {
			$new_data = array(
					'buttons' => '<form action="" method="post">' .
						$this->getUnlinkSkupinaButton($array['s_id']) .
						$this->getEditLink('/admin/skupiny/edit/' . $array['s_id']) .
						$this->getRemoveLink('/admin/skupiny/remove?u[]=' . $array['s_id']) .
						'</form>',
					'name' => getColorBox($array['s_color_text'], $array['s_description']) . '&nbsp;' . $array['s_name']
			);
			$array = $new_data;
		}unset($array);
		
		$skupinyNotInGroup = DBSkupiny::getNotInGroup($id);
		$skupinySelect = array();
		foreach($skupinyNotInGroup as $array) {
			$skupinySelect[$array['s_id']] = $array['s_name'];
		}
		
		$this->render('files/View/Admin/Platby/CategoryGroupForm.inc', array(
				'id' => $id,
				'action' => $action,
				'category' => $data,
				'categorySelect' => $categorySelect,
				'skupiny' => $skupiny,
				'skupinySelect' => $skupinySelect
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
	private function getOrphanGroupSkupina() {
		$out = DBPlatbyGroup::getWithoutSkupina();
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
	private function getOrphanGroupCategory() {
		$out = DBPlatbyGroup::getWithoutCategory();
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
	private function getUnlinkGroupButton($id) {
		return
			'<input type="hidden" name="group" value="' . $id . '">' .
			'<button name="action" value="group_remove">' .
				'<img alt="Odstranit spojení" src="/images/unlink.png" />' .
			'</button>';
	}
	private function getUnlinkCategoryButton($id) {
		return
			'<input type="hidden" name="category" value="' . $id . '">' .
			'<button name="action" value="category_remove">' .
				'<img alt="Odstranit spojení" src="/images/unlink.png" />' .
			'</button>';
	}
	private function getUnlinkSkupinaButton($id) {
		return
			'<input type="hidden" name="skupina" value="' . $id . '">' .
			'<button name="action" value="skupina_remove">' .
				'<img alt="Odstranit spojení" src="/images/unlink.png" />' .
			'</button>';
	}
	private function checkCategoryPost() {
		$f = new Form();
		$dueDate = $this->date('dueDate')->getPost();
		if($dueDate->getYear() == '0000')
			$dueDate = str_replace('0000', '2000', (string) $dueDate);
		$f->checkDate($dueDate, 'Datum splatnosti není platné.');
		
		$validRange = $this->date('validRange')->range()->getPostRange();
		if($validRange['from']->getYear() == '0000') {
			$f->checkDate(str_replace('0000', '2000', (string) $validRange['from']), 'Datum platnosti není platné');
			if($validRange['to']->isValid() && $validRange['to']->getYear() == '0000')
				$f->checkDate(str_replace('0000', '2000', (string) $validRange['to']), 'Datum platnosti (část \'do\') není platné');
			else
				$f->checkDate((string) $validRange['from'], 'Datum platnosti (část \'do\') není platné');
		} else {
			$f->checkDate((string) $validRange['from'], 'Datum platnosti není platné');
			if($validRange['to']->isValid())
				$f->checkDate((string) $validRange['from'], 'Datum platnosti (část \'do\') není platné');
		}
		$f->checkNotEmpty(post('name'), 'Zadejte prosím nějaké jméno.');
		$f->checkNumeric(post('symbol'), 'Zadejte prosím platný specifický symbol.');
		$f->checkRegexp(post('amount'), '/(\*)?([0-9]+)([.,][0-9]+)?/', 'Zadejte prosím platnou očekávanou částku.');
		
		return $f->isValid() ? true : $f;
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