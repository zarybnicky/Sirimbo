<?php
include_once('files/Controller/Admin/Platby.php');
class Controller_Admin_Platby_Manual extends Controller_Admin_Platby {
	function __construct() {
		Permissions::checkError('platby', P_OWNED);
	}
	function view($id = null) {
		if(!empty($_POST)) {
			$this->_processPost();
		}
		
		$remaining = DBPlatbyRaw::getUnsorted();
		$remainingCount = count($remaining);
		if($remainingCount == 0) {
			$this->redirect('/admin/platby/discarded', 'Nezbývají už žádné nezatříděné platby');
		}
		$users = DBUser::getUsersLookup();
		$categories = DBPlatbyGroup::getGroupsWithCategories();
		$raw = unserialize($remaining[0]['pr_raw']);
		
		$new = array();
		$group_id = 0;
		foreach($categories as $array) {
			if($group_id != $array['pg_id'])
				$new['group_' . $array['pg_id']] = "{$array['pg_name']}:";
			$new[(int) $array['pc_symbol']] = "{$array['pc_symbol']} - {$array['pc_name']}";
		}
		$categories = $new;
		
		foreach($users as $key => &$array) {
			$new_data = User::var_symbol($array['u_id']) . " - {$array['u_prijmeni']}, {$array['u_jmeno']}";
			$array = $new_data;
		}
		ksort($users);
		
		$specific = $variable = $date = $amount = null;
		foreach($raw as $key => $value) {
			if($specific === null && mb_stripos($key, 'specif') !== false)
				$specific = array($key, $value);
			if($variable === null && mb_stripos($key, 'variab') !== false)
				$variable = array($key, $value);
			if($date === null && mb_stripos($key, 'datum') !== false)
				$date = array($key, $value);
			if($amount === null && mb_stripos($key, 'částka') !== false)
				$amount = array($key, $value);
		}
		if($variable !== null && !isset($users[(int) $variable[1]]))
			$variable = array($variable[0], "&nbsp;--- (není v DB: {$variable[1]})");
		if($specific !== null && !isset($categories[(int) $specific[1]]))
			$specific = array($specific[0], "&nbsp;--- (není v DB: {$specific[1]})");
		
		foreach(array('specific', 'variable', 'date', 'amount') as $name) {
			if($$name === null) {
				$recognized[$name] = array('column' => '&nbsp;---', 'value' => '&nbsp;---');
			} else {
				$recognized[$name] = array('column' => ${$name}[0], 'value' => ${$name}[1]);
				$$name = ${$name}[1];
			}
		}
		
		$new = array();
		foreach($raw as $key => &$value) {
			$new[] = array(
					'column' => $key,
					'value' => $value
			);
		}
		$raw = $new;
		
		$this->render('files/View/Admin/Platby/ManualSingle.inc', array(
				'id' => $remaining[0]['pr_id'],
				'remainingTotal' => $remainingCount,
				'raw' => $raw,
				'guess' => array(
						'specific' => $specific,
						'variable' => $variable,
						'date' => $date,
						'amount' => $amount
				),
				'users' => $users,
				'categories' => $categories,
				'recognized' => $recognized
		));
	}
	private function _processPost() {
		if(!post('id') || !($current = DBPlatbyRaw::getSingle(post('id')))) {
			$this->redirect()->setRedirectMessage('Zadaná platba neexistuje.');
			return;
		} elseif($current['pr_sorted'] && ($item = DBPlatbyItem::getSingleByRawId(post('id')))) {
			$this->redirect()->setRedirectMessage('Zadaná platba už byla zařazená.');
			return;
		} elseif($current['pr_discarded']) {
			$this->redirect()->setRedirectMessage('Zadaná platba neexistuje.');
			return;
		}
		if(post('action') == 'confirm') {
			$userLookup = DBUser::getUsersLookup();
			$categoryLookup = DBPlatbyCategory::getCategoryLookup();
			
			if(!isset($userLookup[(int) post('variable')]) ||
					!isset($categoryLookup[(int) post('specific')]) ||
					!($date = (string) new Date(post('date')))) {
				dump(array(!isset($userLookup[(int) post('variable')]), !isset($categoryLookup[(int) post('specific')]), !($date = (string) new Date(post('date')))));
				dump($categoryLookup);
				$this->redirect()->setRedirectMessage('Neexistující ID uživatele/kategorie nebo neplatné datum.');
				return;
			}
			$variable = (int) post('variable');
			$specific = (int) post('specific');
			$amount = (int) post('amount');
			
			dump(array('confirm', array($variable, $specific, post('id'), $amount, $date)));
			DBPlatbyRaw::update(post('id'), $current['pr_raw'], $current['pr_hash'], '1', '0');
			DBPlatbyItem::insert($variable, $specific, post('id'), $amount, $date);
		} elseif(post('action') == 'discard') {
			dump(array('discard', array(post('id'), $current['pr_hash'], '0', '1')));
			DBPlatbyRaw::update(post('id'), $current['pr_raw'], $current['pr_hash'], '0', '1');
		} else {
			$this->redirect()->setRedirectMessage('Neplatná POST akce.');
		}
	} 
}