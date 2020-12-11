<?php
class Renderer
{
    use HelperTrait;

    protected $template = [];
    protected $cache = [];
    protected $vars = [];

    public function __get($key)
    {
        if (!isset($this->vars[$key])) {
            fwrite(fopen('php://stderr', 'w'), "Could not find variable '$key' in template\n");
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

    public function render($name, array $vars = [])
    {
        $file = ROOT . DIRECTORY_SEPARATOR . $name;
        array_push($this->template, $file);

        array_push($this->cache, $this->vars);
        $this->vars = $vars;

        if (!file_exists($file)) {
            fwrite(fopen('php://stderr', 'w'), "Could not find file $file to render\n");
            throw new NotFoundRightException("Soubor nebyl nalezen!");
        }
        ob_start();
        include $file;

        $this->vars = array_pop($this->cache);
        array_pop($this->template);

        return ob_get_clean();
    }
}
