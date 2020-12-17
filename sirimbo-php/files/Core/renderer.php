<?php
class Renderer
{
    protected $template = [];
    protected $cache = [];
    protected $vars = [];

    public function __get($key)
    {
        if (!isset($this->vars[$key])) {
            syslog(LOG_WARNING, "Could not find variable '$key' in template\n");
            return null;
        }
        return $this->vars[$key];
    }

    public function __set($key, $val)
    {
        $this->vars[$key] = $val;
    }

    public function __isset($key)
    {
        return isset($this->vars[$key]);
    }

    public function render($tplName, array $bindVars = [])
    {
        $__file = ROOT . DIRECTORY_SEPARATOR . $tplName;
        array_push($this->template, $__file);

        array_push($this->cache, $this->vars);
        $this->vars = $bindVars;

        if (!file_exists($__file)) {
            syslog(LOG_WARNING, "Could not find file $__file to render\n");
            throw new NotFoundException("Soubor nebyl nalezen!");
        }
        ob_start();
        extract($this->vars, EXTR_SKIP);
        include $__file;

        $this->vars = array_pop($this->cache);
        array_pop($this->template);

        return ob_get_clean();
    }
}
