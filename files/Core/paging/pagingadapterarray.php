<?php
class PagingAdapterArray implements PagingAdapterInterface {
	private $array;
	
	function __construct($a = null) {
		$this->setArray($a);
	}
	function setArray($a) {
		if(!is_array($a))
			return false;
		$this->array = $a;
	}
	
	function page($offset, $length, $options = '') {
		return array_slice($this->array, $offset, $lenght);
	}
	function count() {
		return count($this->array);
	}
}