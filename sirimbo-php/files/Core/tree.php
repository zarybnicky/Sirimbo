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

    public function getChildren()
    {
        return $this->children;
    }

    public function add($element)
    {
        $this->children[] = $element;
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
