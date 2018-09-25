<?php
class HeaderHelper
{
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
            ['class' => 'header-section'],
            new Tag(
                'div',
                ['class' => 'container full'],
                [
                    new Tag('h1', [], $this->header),
                    ($this->subheader
                     ? new Tag('h2', [], $this->subheader)
                     : '')
                ]
            )
        );
    }

    public function __toString()
    {
        return $this->render();
    }
}
