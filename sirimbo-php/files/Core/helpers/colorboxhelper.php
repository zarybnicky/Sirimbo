<?php
class ColorboxHelper
{
    protected $color;
    protected $description;

    public function __construct($color, $description)
    {
        $this->color = $color;
        $this->description = $description;
    }

    public function __toString()
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
}
