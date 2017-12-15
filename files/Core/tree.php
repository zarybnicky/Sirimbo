<?php
class Tree
{
    protected $name;
    protected $value;
    protected $children;

    public function __construct($name, $value, $children = [])
    {
        $this->name = $name;
        $this->value = $value;
        $this->children = $children;
    }

    public function getName()
    {
        return $this->name;
    }

    public function getValue()
    {
        return $this->value;
    }

    public function getChildren()
    {
        return $this->children;
    }

    public function find($name)
    {
        return isset($this->children[$name]) ? $this->children[$name] : false;
    }

    public function add(Tree $element)
    {
        $this->children[] = $element;
    }

    public function remove(Tree $element)
    {
        $key = array_search($this->children, $element);
        if ($key) {
            unset($this->children[$key]);
        }
    }

    public function addAt($n, Tree $element)
    {
        array_splice($this->children, $n, 0, [$element]);
    }

    public function removeAt($n)
    {
        array_splice($this->children, $n, 1);
    }

    public function update($fn)
    {
        $this->value = $fn($this->value, $this);
    }

    public function traverse($fn)
    {
        $stack = [$this];
        while ($stack) {
            $current = array_pop($stack);
            if ($current instanceof Tree) {
                $current->update($fn);
                if ($children = $current->getChildren()) {
                    for (end($children); key($children) !== null; prev($children)) {
                        $stack[] = current($children);
                    }
                }
            }
        }
    }

    public function flattenPostorder()
    {
        $result = [];
        $stack = [$this];
        $ancestors = [];
        while ($stack) {
            $x = end($stack);
            if ($children = $x->getChildren()) {
                if ($x !== end($ancestors)) {
                    $ancestors[] = $x;
                    for (end($children); key($children) !== null; prev($children)) {
                        $stack[] = current($children);
                    }
                    continue;
                }
                array_pop($ancestors);
            }
            $result[] = $x->getValue();
            array_pop($stack);
        }
        return $result;
    }

    public function __toString()
    {
        $children = '';
        foreach ($this->getChildren() as $child) {
            $children .= "\n" . implode(
                "\n", array_map(
                    function ($x) {
                        return '  ' . $x;
                    },
                    explode("\n", (string) $child)
                )
            );
        }
        return "{$this->name} (" . gettype($this->value) . ")$children";
    }
}
