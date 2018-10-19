<?php
class CheckboxHelper
{
    protected $name;
    protected $value;
    protected $state;
    protected $readonly;
    protected $label;
    protected $labelCls;
    protected $cls;

    public function checkbox($name, $value = null)
    {
        if ($value === null) {
            $value = $name;
        }

        $this->name = $name;
        $this->value = $value;

        $this->state = false;
        $this->readonly = false;

        $this->label = null;
        $this->cls = 'form-check';

        return $this;
    }

    public function set($val)
    {
        $this->state = (bool) $val;
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

    public function render()
    {
        $checkbox = new Tag(
            'input',
            [
                'type' => 'checkbox',
                'class' => 'form-check-input',
                'name' => $this->name,
                'value' => $this->value,
                'checked' => $this->state,
                'readonly' => $this->readonly
            ]
        );
        return "<div class='{$this->cls}'>"
            . $checkbox
            . ($this->label ? "<label class='form-check-label'>{$this->label}</label>" : '')
            . "</div>";
    }

    public function __toString()
    {
        return $this->render();
    }
}
