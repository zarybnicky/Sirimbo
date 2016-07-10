<?php
class SubheaderHelper
{
    protected $text;

    public function subheader($text)
    {
        $this->text = $text;
        return $this;
    }

    public function render()
    {
        return (string) new Tag(
            'div',
            array('class' => 'container full'),
            new Tag('h2', array(), $this->text)
        );
    }

    public function __toString()
    {
        return $this->render();
    }
}
