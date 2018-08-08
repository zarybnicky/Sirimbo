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
        $this->labelCls = 'form-check-label';
        $this->cls = 'form-check-input';

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

    public function labelCls($labelCls)
    {
        $this->labelCls = $labelCls;
        return $this;
    }

    public function cls($str)
    {
        $this->cls = $str;
        return $this;
    }

    public function render()
    {
        if (!$this->label) {
            return new Tag(
                'input',
                [
                    'type' => 'checkbox',
                    'name' => $this->name,
                    'value' => $this->value,
                    'checked' => $this->state,
                    'readonly' => $this->readonly
                ]
            );
        }
        $checkbox = new Tag(
            'input',
            [
                'type' => 'checkbox',
                'name' => $this->name,
                'value' => $this->value,
                'checked' => $this->state,
                'readonly' => $this->readonly,
                'class' => $this->cls
            ]
        );
        return "<label class='{$this->labelCls}'>$checkbox {$this->label}</label>";
    }

    public function __toString()
    {
        return $this->render();
    }
}
