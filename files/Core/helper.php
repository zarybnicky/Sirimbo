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
		
		if(!isset(self::$registry[$class])) {
			if(!isset(self::$registry))
				self::$registry = array();
			self::$registry[$class] = new $class();
		}
		
		self::$c = &self::$registry[$class];

		if(isset($a[0]) && $a[0] === false && count($a) == 1)
			return self::$c;
		
		switch(count($a)) {
			case 0: return self::$c->{$name}(); break;
			case 1: return self::$c->{$name}($a[0]); break;
			case 2: return self::$c->{$name}($a[0], $a[1]); break;
			case 3: return self::$c->{$name}($a[0], $a[1], $a[2]); break;
			case 4: return self::$c->{$name}($a[0], $a[1], $a[2], $a[3]); break;
			case 5: return self::$c->{$name}($a[0], $a[1], $a[2], $a[3], $a[4]); break;
			default: call_user_func_array(array(self::$c, $name), $a);  break;
		}
	}
	public function __construct() {}
	public static function get() {
		if(!isset(self::$instance))
			self::$instance = new Helper();
		return self::$instance;
	}
}
?>