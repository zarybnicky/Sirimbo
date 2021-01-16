<?php
class Tree
{
    /**
     * @var string
     */
    protected $name;

    protected $value;

    /**
     * @var Tree[]
     */
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

    public function add($element)
    {
        $this->children[] = $element;
    }

    public function remove(Tree $element)
    {
        $key = array_search($element, $this->children);
        if ($key) {
            unset($this->children[$key]);
        }
    }

    public function update($fn)
    {
        $this->value = $fn($this->value, $this);
    }

    public function traverse($fn)
    {
        $stack = [$this];
        while ($stack) {
            /** @var Tree|null */
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

    public function __toString()
    {
        $children = '';
        foreach ($this->getChildren() as $child) {
            $children .= "\n" . implode(
                "\n",
                array_map(fn($x) => "  $x", explode("\n", (string) $child)),
            );
        }
        return "{$this->name} (" . gettype($this->value) . ")$children";
    }
}
