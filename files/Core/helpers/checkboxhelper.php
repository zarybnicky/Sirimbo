<?php
class CheckboxHelper
{
    protected $name;
    protected $value;
    protected $state;
    protected $readonly;
    protected $label;

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

    public function render()
    {
        $checkbox = new Tag(
            'input',
            [
                'type' => 'checkbox',
                'name' => $this->name,
                'value' => $this->value,
                'checked' => $this->state,
                'readonly' => $this->readonly
            ]
        );
        if (!$this->label) {
            return (string) $checkbox;
        }
        return "<label>$checkbox {$this->label}</label>";
    }

    public function __toString()
    {
        return $this->render();
    }
}
