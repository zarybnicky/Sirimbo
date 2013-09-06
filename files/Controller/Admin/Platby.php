<?php
include_once('files/Controller/Admin.php');
class Controller_Admin_Platby extends Controller_Admin {
	function __construct() {
		Permissions::checkError('platby', P_OWNED);
	}
	function view($id = null) {
		$this->render('files/View/Admin/Platby/OverviewNavigation.inc');
	}
	protected function recognizeHeaders($headers, &$specific, &$variable, &$date, &$amount) {
		foreach($headers as $key => $value) {
			if(mb_stripos($value, 'specif') !== false)
				$specific = $value;
			if(mb_stripos($value, 'variab') !== false)
				$variable = $value;
			if(mb_stripos($value, 'datum') !== false)
				$date = $value;
			if(mb_stripos($value, 'částka') !== false)
				$amount = $value;
		}
	}
	protected function checkHeaders($headers, &$specific, &$variable, &$date, &$amount) {
		$headers = array_flip($headers);

		if(!isset($headers[$specific]) || !isset($headers[$variable]) || !isset($headers[$date]) || !isset($headers[$amount]))
			return false;
		else
			return true;
	}
	protected function getCategoryLookup($useSymbolKey, $unique, $includeGroups) {
		$in = DBPlatbyGroup::getGroupsWithCategories();
		$out = array();
		$group_id = 0;
		foreach($in as $array) {
			$key = (int) ($useSymbolKey ? $array['pc_symbol'] : $array['pc_id']);
			
			if($unique && isset($out[$key]))
				continue;
			
			if($includeGroups && $group_id != $array['pg_id'] && !isset($out['group_' . $array['pg_id']])) {
				$out['group_' . $array['pg_id']] = $array;
				$group_id = $array['pg_id'];
			}
			$out[$key] = $array;
		}
		return $out;
	}
	protected function getUserLookup($sort) {
		$in = DBUser::getUsers();
		if($sort) {
			usort($in, function($a, $b) {
				$c = $a['u_prijmeni'];
				$d = $b['u_prijmeni'];
				return ($c > $d ? 1 : ($c < $d ? -1 : 0));
			});
		}
		$out = array();
		foreach($in as $array) {
			$out[(int) $array['u_id']] = $array;
		}
		return $out;
	}
	protected function formatData($specific, $variable, $date, $amount) {
		$specific = (int) $specific;
		$variable = (int) $variable;
		$date = (string) new Date($date);
		$amount = str_replace(',', '.', $amount);dump(array($amount, floatval($amount)));
		if(is_float(floatval($amount))) {
			$amount = number_format(floatval($amount), 2, '.', '');
		} else
			$amount = '0.00';
		
		return array($specific, $variable, $date, $amount);
	}
	protected function checkPost() {
		$userLookup = $this->getUserLookup(false);
		$categories = $this->getCategoryLookup(false, true, false);
		
		$s = array();
		if(!isset($userLookup[(int) post('variable')]))
			$s[] = 'Neplatné ID uživatele';
		if(!isset($categories[(int) post('specific')]))
			$s[] = 'Neplatné ID kategorie';
		if(!($date = (string) new Date(post('date'))))
			$s[] = 'Neplatné datum';
		return $s;
	}
}
?>