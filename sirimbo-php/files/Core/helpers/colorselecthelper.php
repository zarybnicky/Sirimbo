<?php
class ColorSelectHelper
{
    protected $name;
    protected $value;

    public function __construct($name, $value = null)
    {
        $this->name = $name;
        $this->value = $value ?: null;
    }

    public function __toString()
    {
        return "<input type=color name='{$this->name}' value='{$this->value}'>";
    }
}
