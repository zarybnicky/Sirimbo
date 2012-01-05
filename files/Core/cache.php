<?php
class Cache {
	private $content;
	private $valid = false;
	private $name = '';
	private $time;
	
	function __construct($name, $content = false) {
		$this->reset($name, $content);
	}
	
	function reset($name, $new = false) {
		$this->name = $name;
		$this->content = $new;
		$this->valid = true;
		$this->time = time();
	}
	function set($new) {
		$this->content = $new;
		$this->valid = true;
		$this->time = time();
	}
	function get() {
		if($this->valid == true)
			return $this->content;
		else
			return false;
	}
	function setName($name) {
		$this->name = $name;
	}
	function getName() {
		return $this->name;
	}
	function getTime() {
		return $this->time;
	}
	function invalidate() {
		$this->valid = false;
	}
	function isValid() {
		return $this->valid;
	}
}
?>