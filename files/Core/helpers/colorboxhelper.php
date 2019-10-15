<?php
class ColorboxHelper
{
    protected $color;
    protected $description;

    public function colorbox($color, $description)
    {
        $this->color = $color;
        $this->description = $description;

        return $this;
    }

    public function render()
    {
        return (string) new Tag(
            'div',
            [
                'class' => 'box',
                'title' => $this->description,
                'style' => 'background-color:' . $this->color
            ]
        );
    }

    public function __toString()
    {
        return $this->render();
    }
}
