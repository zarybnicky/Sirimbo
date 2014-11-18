<?php
class HeaderHelper {
    protected $header;
    protected $subheader;

    public function header($header) {
        $this->header = $header;
        $this->subheader = null;
        return $this;
    }

    public function subheader($subheader) {
        $this->subheader = $subheader;
        return $this;
    }
    
    public function render() {
        $text = '<h1>' . $this->header . '</h1>';

        if ($this->subheader) {
            $text .= '<h2>' . $this->subheader . '</h2>';
        }

        return
            '<div class="header-section">' .
            '<div class="container full">' .
            $text .
            '</div>' .
            '</div>';
    }
    
    public function __toString() {
        return $this->render();
    }
}