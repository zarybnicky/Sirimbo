<?php
class BsRadioHelper
{
    protected $name;
    protected $value;
    protected $state;
    protected $readonly;
    protected $label;

    public function bsRadio($name, $value = null)
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
        $radio = new Tag(
            'input',
            [
                'type' => 'radio',
                'class' => 'form-check-input',
                'name' => $this->name,
                'value' => $this->value,
                'checked' => $this->state,
                'readonly' => $this->readonly
            ]
        );
        if (!$this->label) {
            return (string) $radio;
        }
        return "<div class='form-check'><label class='form-check-label'>$radio {$this->label}</label></div>";
    }

    public function __toString()
    {
        return $this->render();
    }
}
