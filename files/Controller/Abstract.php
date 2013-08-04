<?php
abstract class Controller_Abstract implements Controller_Interface {
	abstract function view($id = null);
	
	function sidebar() {
		$parent = get_parent_class($this);
		if($parent && $parent != 'Controller_Abstract' && method_exists($parent, "sidebar"))
			return call_user_method('sidebar', new $parent);
		return '';
	}
}