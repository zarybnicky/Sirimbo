<?php
abstract class Controller_Abstract implements Controller_Interface {
	abstract public function view($id = null);
	
	public function sidebar() {
		$parent = get_parent_class($this);
		if($parent && $parent != 'Controller_Abstract' && method_exists($parent, "sidebar")) {
			return call_user_method('sidebar', new $parent);
		}
		return '';
	}
	
	public function render($filename, array $vars = array(), $standalone = false) {
		$r = new Renderer();
		$content = $r->render($filename, $vars);
		
		if($standalone) {
			echo $content;
			return;
		}
		include(TISK ? HEADER_TISK : HEADER);
		echo $content;
		include(TISK ? FOOTER_TISK : FOOTER);
	}
	public function __call($name, $args) {
		$trace = debug_backtrace();
		$class = (isset($trace[1]['class']) ? $trace[1]['class'] : NULL);
		
		if(is_subclass_of($class, __CLASS__)) {
			if(empty($args))
				return Helper::get()->$name();
			else
				return call_user_func_array(array(Helper::get(), $name), $args);
		} else {
			throw new BadMethodCallException("Neplatná akce $name pro ${__CLASS__}");
		}
	}
}