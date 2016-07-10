<?php
class HeaderHelper {
    protected $header;
    protected $subheader;

    public function header($header, $subheader = null)
    {
        $this->header = $header;
        $this->subheader = $subheader;
        return $this;
    }

    public function render()
    {
        return (string) new Tag(
            'div',
            array('class' => 'header-section'),
            new Tag(
                'div',
                array('class' => 'container full'),
                array(
                    new Tag('h1', array(), $this->header),
                    ($this->subheader
                     ? new Tag('h2', array(), $this->subheader)
                     : '')
                )
            )
        );
    }

    public function __toString()
    {
        return $this->render();
    }
}