<?php
class Date {
	private $valid;
	private $sql_format;
	private $year;
	private $month;
	private $day;
	
	const FORMAT_SQL = 'yyyy-mm-dd';
	const FORMAT_SIMPLE = 'dd.mm.yyyy';
	const FORMAT_SIMPLE_SPACED = 'dd. mm. yyyy';
	const FORMAT_SIMPLIFIED = 'd. m. yyyy';
	const FORMAT_SLASHED = 'dd/mm/yyyy';
	
	function __construct($s = null) {
		if(is_string($s))
			$this->setDate($s);
	}
	function __toString() {
		return $this->getDate();
	}
	function setDate($s) {
		if(strpos($s, '-') !== false) {
			$pieces = explode('-', $s);
		} elseif(strpos($s, '.') !== false) {
			$pieces = explode('.', $s);
		} elseif(strpos($s, '/') !== false) {
			$pieces = explode('/', $s);
		}
		if(!isset($pieces) || count($pieces) != 3) {
			if(!$this->sql_format)
				$this->valid = false;
			return false;
		}
		
		foreach($pieces as &$piece) {
			$piece = trim($piece, '/-. ');
			if(strlen($piece) == 1)
				$piece = '0' . $piece;
		}
		
		if(strlen($pieces[0]) == 4) {
			$this->year = $pieces[0];
			$this->month = $pieces[1];
			$this->day = $pieces[2];
			$this->sql_format = implode('-', $pieces);
		} elseif(strlen($pieces[2]) == 4) {
			$this->year = $pieces[2];
			$this->month = $pieces[1];
			$this->day = $pieces[0];
			$this->sql_format = implode('-', array_reverse($pieces));
		} else {
			if(!$this->sql_format)
				$this->valid = false;
			return false;
		}
		$this->valid = true;
		return true;
	}
	function getDate($format = Date::FORMAT_SQL) {
		if(!$this->valid)
			return '';
		switch($format) {
			case Date::FORMAT_SQL:
				return $this->sql_format;
			case Date::FORMAT_SIMPLE:
				return $this->day . '.' . 	$this->month . '.' . $this->year;
				break;
			case Date::FORMAT_SIMPLE_SPACED:
				return $this->day . '. ' . 	$this->month . '. ' . $this->year;
				break;
			case Date::FORMAT_SIMPLIFIED:
				return ((int) $this->day) . '. ' .
					((int) $this->month) . '. ' . $this->year;
				break;
			case Date::FORMAT_SLASHED:
				return $this->day . '/' . 	$this->month . '/' . $this->year;
				break;
			default:
				return '';
		}
	}
	function getDay() {
		return $this->day;
	}
	function getMonth() {
		return $this->month;
	}
	function getYear() {
		return $this->year;
	}
	function isValid() {
		return $this->valid;
	}
}