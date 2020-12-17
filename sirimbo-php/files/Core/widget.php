<?php
class Widget extends Tree
{
    protected $stylesheets = [];
    protected $scripts = [];

    public function __construct(array $attributes = [])
    {
        $children = [];
        $childrenInput = array_slice(func_get_args(), 1);
        array_walk_recursive(
            $childrenInput,
            function ($a) use (&$children) {
                $children[] = $a;
            }
        );
        parent::__construct(get_called_class(), $attributes, $children);
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

    public function render()
    {
        return '';
    }

    public function __toString()
    {
        return $this->renderChild([$this->render()]);
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
