<?php
class NoticeHelper {
    protected $text;

    public function notice($text) {
        $this->text = $text;

        return $this;
    }

    public function render() {
        if (!$this->text) {
            return '';
        }

        return '<div class="notice">' . $this->text . '</div>' . "\n";
    }

    public function __toString() {
        return $this->render();
    }
}
