<?php
class SubheaderHelper {
    protected $text;

    public function subheader($text) {
        $this->text = $text;
        return $this;
    }
    
    public function render() {
        return
            '<div class="container full">' .
            '<h2>' .
            $this->text .
            '</h2>' .
            '</div>';
    }
    
    public function __toString() {
        return $this->render();
    }
}
