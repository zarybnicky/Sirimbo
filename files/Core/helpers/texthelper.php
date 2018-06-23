<?php
class TextHelper
{
    protected $name;
    protected $value;
    protected $size;
    protected $cls;

    public function text($name, $value = null)
    {
        if ($value === null) {
            $value = $name;
        }

        $this->name = $name;
        $this->value = $value;
        $this->size = null;
        $this->cls = 'form-control';

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

    public function render()
    {
        return (string) new Tag(
            'input',
            [
                'type' => 'text',
                'class' => $this->cls,
                'size' => $this->size,
                'name' => $this->name,
                'value' => $this->value
            ]
        );
    }

    public function __toString()
    {
        return $this->render();
    }
}
