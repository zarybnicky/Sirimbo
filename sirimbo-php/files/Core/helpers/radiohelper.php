<?php
class RadioHelper
{
    protected $name;
    protected $value;
    protected $state;
    protected $readonly;
    protected $label;
    protected $cls;

    public function __construct($name, $value = null, $state = false)
    {
        if ($value === null) {
            $value = $name;
        }

        $this->name = $name;
        $this->value = $value;

        $this->state = $state;
        $this->readonly = false;

        $this->label = null;
        $this->cls = 'form-check';
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

    public function inline($val = true)
    {
        $this->cls = $val ? 'form-check form-check-inline' : 'form-check';
        return $this;
    }

    public function __toString()
    {
        $checked = $this->state ? 'checked="checked"' : '';
        $readonly = $this->state ? 'readonly="readonly"' : '';
        return "<div class='{$this->cls}'>"
            . "<input type=radio class='form-check-input' name='{$this->name}'"
            . " value='{$this->value}' $checked $readonly>"
            . ($this->label ? "<label class='form-check-label'>{$this->label}</label>" : '')
            . "</div>";
    }
}
