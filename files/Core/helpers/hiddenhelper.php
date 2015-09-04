<?php
class HiddenHelper
{
    protected $name;
    protected $value;

    public function hidden($name, $value = null)
    {
        if ($value === null) {
            $value = $name;
        }

        $this->name = $name;
        $this->value = $value;

        return $this;
    }

    public function set($val)
    {
        $this->state = (bool) $val;
        return $this;
    }

    public function render()
    {
        return '<input type="hidden"'
             . " name=\"{$this->name}\" "
             . " value=\"{$this->value}\" />";
    }

    public function __toString()
    {
        return $this->render();
    }
}
