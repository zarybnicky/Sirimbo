<?php
class Helper
{
    private static $instance;
    private static $registry = array();

    public function __call($name, $args) {
        $class = ucfirst($name) . 'Helper';
        
        if (!class_exists($class)) {
            include CORE . DIRECTORY_SEPARATOR . 'helpers' .
                DIRECTORY_SEPARATOR . strtolower($class) . '.php';
            if (!class_exists($name)) {
                throw new ViewException("Helper '$class' not found.");
            }
        }

        if (!isset(self::$registry[$class])) {
            self::$registry[$class] = new $class();
        }
        return call_user_func_array(array(self::$registry[$class], $name), $args);
    }

    private function __construct() { }

    public static function instance() {
        if (!isset(self::$instance)) {
            self::$instance = new Helper();
        }
        return self::$instance;
    }

    public static function invoke($name, $args) {
        return self::instance()->__call($name, $args);
    }
}
