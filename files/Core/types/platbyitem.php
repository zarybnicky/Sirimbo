<?php
class PlatbyItem {
	public $specific;
	public $variable;
	public $date;
	public $amount;
	public $prefix;
	public $id;
	public $category_id;
	public $isValid;
	
	public function __construct() {
		$this->isValid = false;
	}
	public static function constructFromDB(array $data) {
		$new = new self();
		$new->initFormatted(isset($data['pc_symbol']) ? $data['pc_symbol'] : null, $data['pi_id_user'], $data['pi_date'],
				$data['pi_amount'], $data['pi_prefix'], $data['pi_id'], $data['pi_id_category']);
		$new->isValid = $this->symbol !== null ? true : false;
		return $new;
	}
	public function init($specific, $variable, $date, $amount, $prefix = null, $id = null, $category_id = null) {
		$this->initFormatted($specific, $variable, $date, $amount, $prefix, $id, $category_id);
		$this->isValid = false;
		return $this;
	}
	private function initFormatted($specific, $variable, $date, $amount, $prefix, $id, $category_id) {
		$this->specific = (int) $specific;
		$this->variable = (int) $variable;
		$this->date = (string) new Date($date);
		$amount = str_replace(',', '.', $amount);
		if(is_float(floatval($amount))) {
			$this->amount = number_format(floatval($amount), 2, '.', '');
		} else {
			$this->amount = '0.00';
		}
		$this->prefix = (int) $prefix;
		$this->id = (int) $id;
		$this->category_id = (int) $category_id;
	}
	public function processWithSymbolLookup($userLookup, $categoryLookup) {
		if(!$this->specific && $this->category_id) {
			$category = DBPlatbyCategory::getSingle($this->category_id);
			if($category) {
				$this->specific = $category['pc_symbol']; }
			else {
				$this->specific = null; } }
		elseif($this->specific && !$this->category_id) {
			if(mb_strlen($this->specific) <= 4) {
				if(isset($categoryLookup[$this->specific])) {
					$this->category_id = $categoryLookup[$this->specific]['pc_id']; }
				if(!$this->prefix) {
					$this->prefix = $this->date ? (new Date($this->date))->getYear() : 0; } }
			else {
				$specific = $this->specific;
				if(isset($categoryLookup[$specific])) {
					$this->category_id = $categoryLookup[$specific]['pc_id']; }
				else {
					$specific = substr($specific, 4);
					if(isset($categoryLookup[$specific]) && $categoryLookup[$specific]['pc_use_prefix']) {
						$this->category_id = $categoryLookup[$specific]['pc_id']; } }
				
				if(!$this->prefix) {
					if(isset($categoryLookup[$specific]) && $categoryLookup[$specific]['pc_use_prefix'] && strlen($this->specific) >= 4) {
						$this->prefix = substr($this->specific, 0, 4); }
					else {
						$this->prefix = $this->date ? (new Date($this->date))->getYear() : 0; } } } }
		else {
			$this->specific = null;
			$this->category_id = null; }
		
		if($this->variable && !isset($userLookup[$this->variable])) {
			$this->variable = null; }
		
		if($this->specific && $this->variable && $this->date && $this->amount && $this->prefix && $this->category_id) {
			$this->isValid = true; }
		else {
			$this->isValid = false; }
	}
}