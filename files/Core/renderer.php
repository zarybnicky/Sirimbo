<?php
class Renderer
{
    private $_cache;
    private $_vars;
    private $_file;

    public function __construct() {
        $this->setVars(array());
        $this->_cache = array();
    }
    public function __get($key) {
        if (!isset($this->_vars[$key])) {
            Log::write('Variable "' . $key . '" is not set!');
            return null;
        }
        return $this->_vars[$key];
    }
    public function __set($key, $val) {
        $this->_vars[$key] = $val;
    }
    public function __isset($key) {
        return isset($this->_vars[$key]);
    }
    public function __call($name, $args) {
        if (empty($args))
            return Helper::get()->$name();
        else
            return call_user_func_array(array(Helper::get(), $name), $args);
    }
    public function setVars(array $vars) {
        $this->_vars = $vars;
    }
    public function vars($key = null) {
        if ($key !== null)
            return $this->$$key;
        return $this->_vars;
    }

    public function render($name, array $variables = array()) {
        $this->_cache[] = $this->vars();
        $this->setVars($variables);
        unset($variables);

        $this->_file = ROOT . DIRECTORY_SEPARATOR . $name;
        unset($name);

        extract($this->vars(), EXTR_SKIP);
        if (!file_exists($this->_file)) {
            Log::write('Could not find file "' . $this->_file . '"');
            throw new NotFoundRightException("Soubor nebyl nalezen!");
        }
        ob_start();
        include $this->_file;

        $this->setVars(array_pop($this->_cache));

        return ob_get_clean();
    }
}
?>