<?php
include_once('files/Controller/Admin/Platby.php');
class Controller_Admin_Platby_Discarded extends Controller_Admin_Platby {
	function __construct() {
		Permissions::checkError('platby', P_OWNED);
	}
	function view($id = null) {
		$data = DBPlatbyRaw::getDiscarded();
		if(count($data) == 0) {
			$this->redirect('/admin/platby/overview', 'V DB nejsou žádné vyřazené platby.');
		}
		if(get('list')) {
			$this->getFiltered($data, $result, $columns, $header);
			$this->render('files/View/Admin/Platby/DiscardedTable.inc', array(
					'data' => $result,
					'columns' => $columns,
					'header' => $header
			));
		} else {
			$this->getList($data, $groupAmount, $groupDate);
			$this->render('files/View/Admin/Platby/DiscardedList.inc', array(
					'groupByDate' => $groupDate,
					'groupByAmount' => $groupAmount
			));
		}
	}
	function getFiltered($data, &$result, &$columns, &$header) {
		if(get('list') == 'date')
			$header = (get('year') == 'none' ? 'nezařazené podle data' :
				(get('month') ? (get('year') . '/' . get('month')) : get('year')));
		elseif(get('list') == 'amount')
			$header = (get('amount') == 'none' ? 'nezařazené podle částky' :
				(get('amount') . ' Kč'));
		else
			$header = 'všechny';
		
		$result = array();
		$columns = array();
		$columnsTemp = array();
		foreach($data as $raw) {
			$row = unserialize($raw['pr_raw']);
			if(!$this->checkHeaders($row, $date, $amount))
				$this->getHeaders($row, $date, $amount);
			
			if(get('list') == 'date') {
				if(isset($row[$date]) && $row[$date]) {
					if(get('year') == 'none')
						continue;
					$currentDate = new Date($row[$date]);
					if($currentDate->getYear() != get('year') ||
							(get('month') && $currentDate->getMonth() != get('month')))
						continue;
				} elseif(get('year') !== 'none') {
					continue;
				}
			} elseif(get('list') == 'amount' && ((!isset($row[$amount]) ^ get('amount') == 'none') || get('amount') != (int) $row[$amount])) {
				continue;
			}
			foreach($row as $key => $value) {
				if($value)
					$columnsTemp[$key] = true;
				elseif(!isset($columnsTemp[$key]))
					$columnsTemp[$key] = false;
			}
			$row['edit'] = '<a href="/admin/platby/manual/' . $raw['pr_id'] . '">Zařadit</a>';
			$result[] = $row;
		}
		if(empty($columnsTemp))
			return;
		
		$columns[] = array('edit', 'Zařadit');
		foreach($columnsTemp as $key => $value) {
			if(!$value)
				continue;
			$columns[] = array($key, $key);
		}
	}
	function getList($data, &$groupAmount, &$groupDate) {
		$groupDate = array();
		$groupAmount = array();
		foreach($data as $row) {
			$row = unserialize($row['pr_raw']);
			if(!$this->checkHeaders($row, $date, $amount))
				$this->getHeaders($row, $date, $amount);
			
			if(isset($row[$date]) && $row[$date]) {
				$currentDate = new Date($row[$date]);
				if(!isset($groupDate[$currentDate->getYear()]))
					$groupDate[$currentDate->getYear()] = array('name' => $currentDate->getYear());
				
				if(!isset($groupDate[$currentDate->getYear()]['months'][$currentDate->getMonth()]))
					$groupDate[$currentDate->getYear()]['months'][$currentDate->getMonth()] =
						$currentDate->getYear() . '/' . $currentDate->getMonth();
			} elseif(!isset($groupDate['none'])) {
				$groupDate['none'] = array('name' => 'Nerozpoznáno');
			}
			
			if(isset($row[$amount])) {
				if(!isset($groupAmount[$row[$amount]]))
					$groupAmount[(int) $row[$amount]] = (int) $row[$amount] . ' Kč';
			} elseif(!isset($groupAmount['none'])) {
				$groupAmount['none'] = 'Nerozpoznáno';
			}
		}
		krsort($groupAmount);
		krsort($groupDate);
		foreach($groupDate as $year)
			krsort($year);
	}
	function getHeaders($associative, &$date, &$amount) {
		foreach($associative as $key => $value) {
			if($date === null && mb_stripos($key, 'datum') !== false)
				$date = $key;
			if($amount === null && mb_stripos($key, 'částka') !== false)
				$amount = $key;
		}
	}
	function checkHeaders($associative, &$date, &$amount) {
		if(!isset($associative[$date]) || !isset($associative[$amount]))
			return false;
		else
			return true;
	}
}