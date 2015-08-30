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
        return
            '<div class="header-section">'
            . '<div class="container full">'
            . "<h1>{$this->header}</h1>"
            . ($this->subheader ? "<h2>$this->subheader</h2>" : '')
            . '</div>'
            . '</div>';
    }

    public function __toString()
    {
        return $this->render();
    }
}