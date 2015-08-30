<?php
class SubmitHelper
{
    protected $text;
    protected $name;
    protected $value;

    public function submit($text)
    {
        $this->text = $text;
        $this->name = null;
        $this->value = null;
        return $this;
    }

    public function name($name)
    {
        $this->name = $name;
        return $this;
    }

    public function value($value)
    {
        $this->value = $value;
        return $this;
    }

    public function data($name, $value)
    {
        $this->name = $name;
        $this->value = $value;
        return $this;
    }

    public function render()
    {
        return '<button type="submit"' .
            ($this->name ? ' name="' . $this->name . '"' : '') .
            ($this->value ? ' value="' . $this->value . '"' : '') .
            '>' . $this->text . '</button>';
    }

    public function __toString()
    {
        return $this->render();
    }
}
