<?php
class Renderer
{
    protected $cache;
    protected $vars;

    public function __construct() {
        $this->vars = [];
        $this->cache = [];
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
        if (isset($this->vars[$name]) && is_callable($this->vars[$name])) {
            return call_user_func_array($this->vars[$name], $args);
        }
        return Helper::invoke($name, $args);
    }

    public function render($name, array $vars = []) {
        array_push($this->cache, $this->vars);
        $this->vars = $vars;
        $file = ROOT . DIRECTORY_SEPARATOR . $name;

        if (!file_exists($file)) {
            Log::write('Could not find file "' . $file . '"');
            throw new NotFoundRightException("Soubor nebyl nalezen!");
        }
        ob_start();
        include $file;

        $this->vars = array_pop($this->cache);

        return ob_get_clean();
    }
}
