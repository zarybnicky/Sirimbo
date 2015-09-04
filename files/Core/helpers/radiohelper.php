<?php
class RadioHelper
{
    protected $name;
    protected $value;
    protected $state;
    protected $readonly;
    protected $label;

    public function radio($name, $value = null)
    {
        if ($value === null) {
            $value = $name;
        }

        $this->name = $name;
        $this->value = $value;

        $this->state = false;
        $this->readonly = false;

        $this->label = null;

        return $this;
    }

    public function set($val)
    {
        $this->state = $val;
        return $this;
    }

    public function label($str)
    {
        $this->label = $str;
        return $this;
    }

    public function readonly($val)
    {
        $this->readonly = $val;
        return $this;
    }

    public function render()
    {
        $out = '<input type="radio"'
             . " name=\"{$this->name}\" "
             . " value=\"{$this->value}\""
             . ($this->state ? ' checked="checked"' : '')
             . ($this->readonly ? ' readonly="readonly"' : '')
             . '/>';
        if ($this->label) {
            $out = "<label>$out {$this->label}</label>";
        }
        return $out;
    }

    public function __toString()
    {
        return $this->render();
    }
}
