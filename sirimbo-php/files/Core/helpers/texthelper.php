<?php
class TextHelper
{
    protected $name;
    protected $value;
    protected $size;
    protected $cls;
    protected $placeholder;
    protected $readonly;
    protected $disabled;

    public function text($name, $value = null)
    {
        if ($value === null) {
            $value = $name;
        }

        $this->name = $name;
        $this->value = $value;
        $this->size = null;
        $this->cls = 'form-control';
        $this->placeholder = null;
        $this->readonly = false;
        $this->disabled = false;

        return $this;
    }

    public function size($size)
    {
        $this->size = $size;
        return $this;
    }

    public function cls($cls)
    {
        $this->cls = $cls;
        return $this;
    }

    public function placeholder($x)
    {
        $this->placeholder = $x;
        return $this;
    }

    public function readonly($x)
    {
        $this->readonly = $x;
        return $this;
    }

    public function disabled($x)
    {
        $this->disabled = $x;
        return $this;
    }

    public function render()
    {
        return (string) new Tag(
            'input',
            [
                'type' => 'text',
                'class' => $this->cls,
                'size' => $this->size,
                'name' => $this->name,
                'value' => $this->value,
                'placeholder' => $this->placeholder,
                'readonly' => $this->readonly,
                'disabled' => $this->disabled
            ]
        );
    }

    public function __toString()
    {
        return $this->render();
    }
}
