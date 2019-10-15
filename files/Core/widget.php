<?php
class Widget extends Tree
{
    protected $stylesheets = [];
    protected $scripts = [];

    public function __construct(array $attributes = [])
    {
        $children = [];
        array_walk_recursive(
            (array_slice(func_get_args(), 1)),
            function ($a) use (&$children) {
                $children[] = $a;
            }
        );
        parent::__construct(
            get_called_class(),
            $attributes,
            $children
        );
    }

    public function get($name = null)
    {
        if ($name === null) {
            return $this->value;
        }
        return isset($this->value[$name]) ? $this->value[$name] : null;
    }

    public function set($key, $value)
    {
        $this->value[$key] = $value;
    }

    public function setAttributes(array $new)
    {
        $this->value = array_merge($this->value, $new);
    }

    public function getStylesheets()
    {
        return $this->stylesheets;
    }

    public function getScripts()
    {
        return $this->scripts;
    }

    public function addStylesheet(array $attributes, $source = '')
    {
        $attributes = array_merge(['rel' => 'stylesheet'], $attributes);
        $this->stylesheets[] = new Tag('style', $attributes, $source);
    }

    public function addScript(array $attributes, $source = '')
    {
        $this->scripts[] = new Tag('script', $attributes, $source);
    }

    public function render()
    {
        return '';
    }

    public function renderToString()
    {
        $result = [$this->render()];
        $pre = [];
        $post = [];
        $this->traverse(
            function ($val, $x) use (&$pre, &$post) {
                if ($x instanceof self) {
                    $pre[] = $x->getStylesheets();
                    $post[] = $x->getScripts();
                }
                return $val;
            }
        );
        return $this->renderChild(array_merge($pre, $result, $post));
    }

    public function __toString()
    {
        return $this->renderToString();
    }

    protected function renderChild($element)
    {
        $result = '';
        $stack = [$element];
        while ($stack) {
            $x = array_pop($stack);
            while ($x instanceof self) {
                $x = $x->render();
            }
            if (is_string($x)) {
                $result .= $x;
                continue;
            }
            if (is_array($x)) {
                if ($x) {
                    for (end($x); key($x) !== null; prev($x)) {
                        $stack[] = current($x);
                    }
                }
                continue;
            }
            trigger_error('Invalid Widget child (' . gettype($x) . '), ignoring.');
        }
        return $result;
    }
}
