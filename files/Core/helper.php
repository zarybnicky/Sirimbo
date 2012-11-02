<?php
class Helper {
	private static $registry;
	private static $c;
	private static $instance;
	
	public function __call($name, $a) {
		$class = ucfirst($name) . 'Helper';
			
		if(!class_exists($class)) {
			include($_SERVER['DOCUMENT_ROOT'] . '/files/Core/helpers/' .
				strtolower($class) . '.php');
				if(!class_exists($name))
					throw new Exception("Helper '$class' not found.");
		}
		
		if(!isset(Helper::$registry[$class])) {
			if(!isset(Helper::$registry))
				Helper::$registry = array();
			Helper::$registry[$class] = new $class();
		}
		
		Helper::$c = &Helper::$registry[$class];

		if(isset($a[0]) && $a[0] === false && count($a) == 1)
			return Helper::$c;
		
		switch(count($a)) {
			case 0: return Helper::$c->{$name}(); break;
			case 1: return Helper::$c->{$name}($a[0]); break;
			case 2: return Helper::$c->{$name}($a[0], $a[1]); break;
			case 3: return Helper::$c->{$name}($a[0], $a[1], $a[2]); break;
			case 4: return Helper::$c->{$name}($a[0], $a[1], $a[2], $a[3]); break;
			case 5: return Helper::$c->{$name}($a[0], $a[1], $a[2], $a[3], $a[4]); break;
			default: call_user_func_array(array(Helper::$c, $name), $a);  break;
		}
	}
	public function __construct() {}
	public static function get() {
		if(!isset(Helper::$instance))
			Helper::$instance = new Helper();
		return Helper::$instance;
	}
}
?>