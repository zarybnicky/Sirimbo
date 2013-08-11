<?php
class Renderer {
	private $__cache;
	private $__vars;
	private $__file;
	private $__content;
	
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
	public function __call($name, $args) {
		return Helper::get()->$name($args);
	}
	public function setVars($vars) {
		if(!is_array($vars)) {
			Log::write('Attempted to set a variable (' . gettype($vars) . ') as an array!');
			return;
		}
		$this->__vars = $vars;
	}
	public function vars($key = null) {
		if($key !== null)
			return $this->$$key;
		return $this->__vars;
	}
	
	public function render($name, $variables = array()) {
		$this->__cache[] = $this->__vars;
		$this->setVars($variables);
		unset($variables);
		
		$this->__file = $name;
		unset($name);
		
		extract($this->vars(), EXTR_SKIP);
		
		if(!file_exists($this->file)) {
			Log::write('Could not find file "' . $this->file . '"');
			View::viewError(ER_NOT_FOUND_RIGHT);
		}
		ob_start();
		include $this->__file;
		return ob_get_clean();
	}
}
?>