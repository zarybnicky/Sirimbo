<?php
class HeaderHelper {
    protected $nadpis;

    public function header($nadpis) {
        $this->nadpis = $nadpis;
        return $this;
    }
    
    public function render() {
        return
            '<div class="header-section">' .
            '<div class="container">' .
            '<h1>' .
            $this->nadpis .
            '</h1>' .
            '</div>' .
            '</div>';
    }
    
    public function __toString() {
        return $this->render();
    }
}