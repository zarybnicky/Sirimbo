<?php
class PagingAdapterDBSelect implements PagingAdapterInterface {
	private $dbadapter;
	private $options;
	
	function __construct($classname, $options = null) {
		$this->setDatabase($classname);
		if($options !== null)
			$this->options = $options;
	}
	function setDatabase($classname) {
		if(!is_a(call_user_func(array($classname, 'getInstance')), 'Pagable'))
			throw new Exception('Database does not implement interface Pageable');
		
		$this->dbadapter = $classname;
	}
	function page($offset, $lenght, $options = '') {
		return $this->dbadapter ?	
			call_user_func_array(array($this->dbadapter, 'getPage'),
				array($offset, $lenght, $this->options))
			: array();
	}
	function count() {
		return $this->dbadapter ?	
			call_user_func(array($this->dbadapter, 'getCount'), $this->options)
			: 0;
	}
}