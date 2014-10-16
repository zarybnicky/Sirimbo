<?php
class Renderer
{
    protected $cache;
    protected $vars;
    protected $file;

    public function __construct() {
        $this->vars = array();
        $this->cache = array();
    }

    public function __get($key) {
        if (!isset($this->vars[$key])) {
            Log::write('Variable "' . $key . '" is not set!');
            return null;
        }
        return $this->vars[$key];
    }

    public function __set($key, $val) {
        $this->vars[$key] = $val;
    }

    public function __isset($key) {
        return isset($this->vars[$key]);
    }

    public function __call($name, $args) {
        return Helper::invoke($name, $args);
    }

    public function render($name, array $vars = array()) {
        array_push($this->cache, $this->vars);
        $this->vars = $vars;
        $this->file = ROOT . DIRECTORY_SEPARATOR . $name;

        if (!file_exists($this->file)) {
            Log::write('Could not find file "' . $this->file . '"');
            throw new NotFoundRightException("Soubor nebyl nalezen!");
        }
        ob_start();
        include $this->file;

        $this->vars = array_pop($this->cache);

        return ob_get_clean();
    }
}
