<?php
include_once('files/Controller/Admin/Platby/Structure.php');
class Controller_Admin_Platby_Structure_Group extends Controller_Admin_Platby_Structure {
	function __construct() {
		Permissions::checkError('platby', P_OWNED);
	}
	function view($id = null) {
		$this->render('files/View/Admin/Platby/StructureGroupOverview.inc', array(
				'data' => $this->getGroups()
		));
	}
	protected function getGroups() {
		$out = array();
		$groups = DBPlatbyGroup::getGroups();
		foreach($groups as $array) {
			$new_data = array();
			$new_data['name'] = $array['pg_name'];
			$new_data['type'] = $array['pg_type'] ? 'Členské příspěvky' : 'Běžné platby'; 
			$new_data['buttons'] = $this->getEditLink('/admin/platby/structure/category/edit/' . $array['pg_id']) . 
				$this->getRemoveLink('/admin/platby/structure/category/remove/' . $array['pg_id']);
			$out[] = $new_data;
		}
		return $out;
	}
	function add($id = null) {
		if(empty($_POST) || is_object($s = $this->checkPost())) {
			if(empty($_POST)) {
				post('base', 1);
			} else {
				$this->redirect()->setMessage($s->getMessages());
			}
			$this->displayForm('add');
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
				$this->redirect('/admin/platby/structure/category/edit/' . get('category'),
						'Kategorie byla přidána, ale nebyla přiřazena - takové přiřazení není platné.');
			}
			$this->redirect('/admin/platby/structure/category/edit/' . get('category'), 'Kategorie úspěšně přidána a přiřazena');
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
		$this->redirect(post('referer') ? post('referer') : '/admin/platby/structure', 'Kategorie úspěšně přidána');
	}
	function edit($id = null) {
		if(!$id || !($data = DBPlatbyGroup::getSingle($id)))
			$this->redirect(post('referer') ? post('referer') : '/admin/platby/structure', 'Kategorie s takovým ID neexistuje');
	
		if(post('action') == 'skupiny') {
			if(!($data = DBSkupiny::getSingle(post('skupiny'))))
				$this->redirect('/admin/platby/structure/group/edit/' . $id, 'Kategorie s takovým ID neexistuje.');
				
			DBSkupiny::addChild(post('skupiny'), $id);
			$conflicts = DBPlatby::checkConflicts(post('skupiny'));
				
			if(!empty($conflicts)) {
				DBSkupiny::removeChild(post('skupiny'), $id);
				$this->redirect('/admin/platby/structure/group/edit/' . $id,
						'Takové přiřazení není platné - způsobilo by, že jeden specifický symbol by byl v jedné skupině dvakrát.');
			}
			$this->redirect('/admin/platby/structure/group/edit/' . $id, 'Kategorie byla úspěšně přiřazena.');
		} elseif(post('action') == 'skupina_remove') {
			if(!($data = DBSkupiny::getSingle(post('skupina'))))
				$this->redirect('/admin/platby/structure/group/edit/' . $id, 'Skupina s takovým ID neexistuje.');
				
			DBSkupiny::removeChild(post('skupina'), $id);
			$this->redirect('/admin/platby/structure/group/edit/' . $id, 'Spojení s kategorií bylo úspěšně odstraněno.');
		} elseif(post('action') == 'category') {
			if(!($data = DBPlatbyCategory::getSingle(post('category'))))
				$this->redirect('/admin/platby/structure/group/edit/' . $id, 'Kategorie s takovým ID neexistuje.');
				
			DBPlatbyGroup::addChild($id, post('category'));
			$skupiny = DBPlatbyGroup::getSingleWithSkupiny($id);
			$conflicts = array();
			foreach($skupiny as $array)
				$conflicts = array_merge($conflicts, DBPlatby::checkConflicts($array['s_id']));
				
			if(!empty($conflicts)) {
				DBPlatbyGroup::removeChild($id, post('category'));
				$this->redirect('/admin/platby/structure/group/edit/' . $id,
						'Takové přiřazení není platné - způsobilo by, že jeden specifický symbol by byl v jedné skupině dvakrát.');
			}
			$this->redirect('/admin/platby/structure/group/edit/' . $id, 'Kategorie byla úspěšně přiřazena.');
		} elseif(post('action') == 'category_remove') {
			if(!($data = DBPlatbyCategory::getSingle(post('category'))))
				$this->redirect('/admin/platby/structure/group/edit/' . $id, 'Specifický symbol s takovým ID neexistuje.');
				
			DBPlatbyGroup::removeChild($id, post('category'));
			$this->redirect('/admin/platby/structure/group/edit/' . $id, 'Spojení se specifickým symbolem bylo úspěšně odstraněno.');
		}
	
		if(empty($_POST) || is_object($s = $this->checkPost())) {
			if(empty($_POST)) {
				post('type', $data['pg_type']);
				post('name', $data['pg_name']);
				post('description', $data['pg_description']);
				post('base', $data['pg_base']);
			} else {
				$this->redirect()->setMessage($s->getMessages());
			}
			$this->displayForm('edit', DBPlatbyGroup::getSingleWithCategories($id));
			return;
		}
	
		DBPlatbyGroup::update($id, post('type'), post('name'), post('description'), post('base'));
		$this->redirect(post('referer') ? post('referer') : '/admin/platby/structure', 'Kategorie úspěšně upravena');
	}
	function remove($id = null) {
		if(!$id || !($data = DBPlatbyGroup::getSingle($id)))
			$this->redirect(post('referer') ? post('referer') : '/admin/platby/structure', 'Kategorie s takovým ID neexistuje');
	
		if(post('action') == 'unlink') {
			$f = $this->getLinkedObjects($id);
				
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
			$this->redirect('/admin/platby/structure/group/remove/' . $id,
					'Spojení s \'' . $skupinaCount . '\' skupinami a s \'' . $categoryCount . '\' kategoriemi bylo odstraněno');
			return;
		}
		if(((empty($_POST) || post('action') == 'confirm') && ($f = $this->getLinkedObjects($id))) || empty($_POST)) {
			if(isset($f) && $f) {
				$this->redirect()->setMessage(
						'Nemůžu odstranit kategorii s připojenými skupinami nebo specifickými symboly! ' .
						'<form action="" method="post"><button type="submit" name="action" value="unlink">Odstranit spojení?</button></form>'
				);
			}
			$this->render('files/View/Admin/Platby/StructureGroupRemove.inc', array(
					'id' => $id,
					'name' => $data['pg_name']
			));
			return;
		}
		DBPlatbyGroup::delete($id);
		$this->redirect(post('referer') ? post('referer') : '/admin/platby/structure', 'Kategorie byla odebrána');
	}
	private function getLinkedObjects($id) {
		$cat = DBPlatbyGroup::getSingleWithCategories($id);
		$sku = DBPlatbyGroup::getSingleWithSkupiny($id);
	
		if(empty($cat) && empty($sku))
			return array();
		else
			return array('categories' => $cat, 'skupiny' => $sku);
	}
	private function displayForm($action, $data = array()) {
		$id = Request::getID() ? Request::getID() : 0;
		foreach($data as $key => &$array) {
			$new_data = array(
					'buttons' => '<form action="" method="post">' .
					$this->getUnlinkCategoryButton($array['pc_id']) .
					$this->getEditLink('/admin/platby/structure/category/edit/' . $array['pc_id']) .
					$this->getRemoveLink('/admin/platby/structure/category/remove/	' . $array['pc_id']) .
					'</form>',
					'name' => $array['pc_name'],
					'specific' => $array['pc_symbol'],
					'amount' => ((float) $array['pc_amount'] * (float) $array['pg_base']),
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
	
		$this->render('files/View/Admin/Platby/StructureGroupForm.inc', array(
				'id' => $id,
				'action' => $action,
				'category' => $data,
				'categorySelect' => $categorySelect,
				'skupiny' => $skupiny,
				'skupinySelect' => $skupinySelect
		));
	}
	protected function checkPost() {
		$f = new Form();
		$f->checkInArray(post('type'), array('0', '1'), 'Neplatný typ kategorie');
		$f->checkNotEmpty(post('name'), 'Zadejte nějaký název platby');
		$f->checkNumeric(post('base'), 'Násobitel musí být zadán pouze čisly');
		
		return $f->isValid() ? true : $f;
	}
}