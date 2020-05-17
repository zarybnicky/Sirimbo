<?php
class SubmitHelper
{
    protected $text;
    protected $name;
    protected $value;
    protected $style;
    protected $cls;

    public function submit($text)
    {
        $this->text = $text;
        $this->name = null;
        $this->value = null;
        $this->style = null;
        $this->cls = 'btn btn-primary';
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

    public function cls($cls)
    {
        $this->cls = $cls;
        return $this;
    }

    public function render()
    {
        return (string) new Tag(
            'button',
            ['name' => $this->name, 'value' => $this->value, 'class' => $this->cls],
            $this->text
        );
    }

    public function __toString()
    {
        return $this->render();
    }
}
