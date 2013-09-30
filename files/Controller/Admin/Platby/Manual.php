<?php
include_once('files/Controller/Admin/Platby.php');
class Controller_Admin_Platby_Manual extends Controller_Admin_Platby {
	function __construct() {
		Permissions::checkError('platby', P_OWNED);
	}
	function view($id = null) {
		if(!empty($_POST)) {
			$this->processPost();
		}
		$remaining = DBPlatbyRaw::getUnsorted();
		$remainingCount = count($remaining);
		
		if($id && ($data = DBPlatbyRaw::getSingle($id))) {
			if($data['pr_sorted'])
				$this->redirect('/admin/platby/discarded', 'Platba už byla zařazena do systému');
			$raw = unserialize($data['pr_raw']);
		} else {
			if($remainingCount == 0) {
				$this->redirect('/admin/platby', 'Nezbývají už žádné nezatříděné platby');
			}
			$id = $remaining[0]['pr_id'];
			$raw = unserialize($remaining[0]['pr_raw']);
		}
		
		$categoriesMap = $this->getCategoryLookup(true, true, false);
		$users = $this->getUserLookup(false);
		
		$this->recognizeHeaders(array_flip($raw), $specific, $variable, $date, $amount);
		$null = '';
		if($specific !== null && $date !== null) {
			$prefix = $this->getPrefix($raw[$specific], $raw[$date]);
		} elseif($specific === null && $date !== null) {
			$prefix = $this->getPrefix($null, $raw[$date]);
		} elseif($specific !== null && $date === null) {
			$prefix = $this->getPrefix($raw[$specific], '');
		} else {
			$prefix = 0;
		}
		$recognized['prefix'] = array('column' => '&nbsp;---', 'value' => $prefix);
		if($variable !== null) {
			if(isset($users[(int) $raw[$variable]]))
				$recognized['variable'] = array('column' => $variable,
						'value' => $users[(int) $raw[$variable]]['u_jmeno'] . ' ' . $users[(int) $raw[$variable]]['u_prijmeni']);
			else
				$recognized['variable'] = array('column' => $variable,
						'value' => "&nbsp;--- (není v DB: {$raw[$variable]})");
			$variable = (int) $raw[$variable];
		}
		if($specific !== null) {
			if(isset($categoriesMap[(int) $raw[$specific]])) {
				$recognized['specific'] = array('column' => $specific,
						'value' => $categoriesMap[(int) $raw[$specific]]['pc_name']);
				$specific = $categoriesMap[(int) $raw[$specific]]['pc_id'];
			} else {
				$recognized['specific'] = array('column' => $specific,
						'value' => "&nbsp;--- (není v DB: {$raw[$specific]})");
				$specific = 0;
			}
		}
		if($date !== null) {
			$recognized['date'] = array('column' => $date, 'value' => (new Date($raw[$date]))->getDate(Date::FORMAT_SIMPLE_SPACED));
			$date = (new Date($raw[$date]))->getDate(Date::FORMAT_SIMPLE_SPACED);
		}
		if($amount !== null) {
			$recognized['amount'] = array('column' => $amount, 'value' => $raw[$amount]);
			$amount = $raw[$amount];
		}
		
		foreach(array('specific', 'variable', 'date', 'amount') as $name) {
			if($$name === null)
				$recognized[$name] = array('column' => '&nbsp;---', 'value' => '&nbsp;---');
		}
		
		$new = array();
		foreach($raw as $key => &$value) {
			$new[] = array(
					'column' => $key,
					'value' => $value
			);
		}
		$raw = $new;
		
		$this->render('files/View/Admin/Platby/ManualForm.inc', array(
				'id' => $id,
				'remainingTotal' => $remainingCount,
				'raw' => $raw,
				'guess' => array(
						'specific' => $specific,
						'variable' => $variable,
						'date' => $date,
						'amount' => $amount,
						'prefix' => $prefix
				),
				'users' => $this->getUsers(),
				'categories' => $this->getCategories(),
				'recognized' => $recognized
		));
	}
	private function getCategories() {
		$categories = parent::getCategoryLookup(false, false, true);
		foreach($categories as $key => &$array) {
			if(strpos($key, 'group_') !== false)
				$array = "{$array['pg_name']}:";
			else
				$array = "{$array['pc_symbol']} - {$array['pc_name']}";
		}
		return $categories;
	}
	private function getUsers() {
		$users = parent::getUserLookup(true);
		foreach($users as &$array) {
			$array = User::var_symbol($array['u_id']) . " - {$array['u_prijmeni']}, {$array['u_jmeno']}";
		}
		return $users;
	}
	private function processPost() {
		if(!post('id') || !($current = DBPlatbyRaw::getSingle(post('id')))) {
			$this->redirect()->setMessage('Zadaná platba neexistuje.');
			return;
		} elseif($current['pr_sorted'] && ($item = DBPlatbyItem::getSingleByRawId(post('id')))) {
			$this->redirect()->setMessage('Zadaná platba už byla zařazená.');
			return;
		}
		if(post('action') == 'confirm') {
			if(($s = $this->checkPost('add')) != array()) {
				$this->redirect()->setMessage($s);
				return;
			}
			list($specific, $variable, $date, $amount, $prefix) =
				$this->formatData(post('specific'), post('variable'), post('date'), post('amount'), post('prefix'));
			
			DBPlatbyRaw::update(post('id'), $current['pr_raw'], $current['pr_hash'], '1', '0');
			DBPlatbyItem::insert($variable, $specific, post('id'), $amount, $date, $prefix);
		} elseif(post('action') == 'discard') {
			if(!$current['pr_discarded'])
				DBPlatbyRaw::update(post('id'), $current['pr_raw'], $current['pr_hash'], '0', '1');
		} elseif(post('action') == 'skip') {
			DBPlatbyRaw::skip(post('id'));
		} else {
			$this->redirect()->setMessage('Neplatná POST akce.');
		}
		$this->redirect('/admin/platby/manual');
	} 
}