<?php
class Renderer {
	private $__cache;
	private $__vars;
	private $__file;
	
	public function __construct() {
		$this->setVars(array());
		$this->__cache = array();
	}
	public function __get($key) {
		if(!isset($this->__vars[$key])) {
			Log::write('Variable "' . $key . '" is not set!');
			return null;
		}
		return $this->__vars[$key];
	}
	public function __set($key, $val) {
		$this->__vars[$key] = $val;
	}
	public function __isset($key) {
		return isset($this->__vars[$key]);
	}
	public function __call($name, $args) {
		if(empty($args))
			return Helper::get()->$name();
		else
			return call_user_func_array(array(Helper::get(), $name), $args);
	}
	public function setVars(array $vars) {
		$this->__vars = $vars;
	}
	public function vars($key = null) {
		if($key !== null)
			return $this->$$key;
		return $this->__vars;
	}
	
	public function render($name, array $variables = array()) {
		$this->__cache[] = $this->vars();
		$this->setVars($variables);
		unset($variables);
		
		$this->__file = $name;
		unset($name);
		
		extract($this->vars(), EXTR_SKIP);
		if(!file_exists($this->__file)) {
			Log::write('Could not find file "' . $this->__file . '"');
			throw new Exception("Soubor nebyl nalezen!");
		}
		ob_start();
		include $this->__file;
		
		$this->setVars(array_pop($this->__cache));
		
		return ob_get_clean();
	}
}
?>