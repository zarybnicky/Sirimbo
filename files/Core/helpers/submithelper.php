<?php
class SubmitHelper
{
    protected $text;
    protected $name;
    protected $value;
    protected $style;

    public function submit($text)
    {
        $this->text = $text;
        $this->name = null;
        $this->value = null;
        $this->style = null;
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

    public function style($style)
    {
        $this->style = $style;
        return $this;
    }

    public function render()
    {
        return (string) new Tag(
            'button',
            array(
                'name' => $this->name,
                'value' => $this->value,
                'style' => $this->style
            ),
            $this->text
        );
    }

    public function __toString()
    {
        return $this->render();
    }
}
