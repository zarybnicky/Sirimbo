<?php
class TextHelper
{
    protected $name;
    protected $value;

    public function text($name, $value = null)
    {
        if ($value === null) {
            $value = $name;
        }

        $this->name = $name;
        $this->value = $value;

        return $this;
    }

    public function render()
    {
        return '<input type="text"'
             . " name=\"{$this->name}\" "
             . " value=\"{$this->value}\" />";
    }

    public function __toString()
    {
        return $this->render();
    }
}
