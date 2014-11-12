<?php 
class SubmitHelper {
    protected $text;

    public function submit($text) {
        $this->text = $text;
        return $this;
    }

    public function render() {
        return
            '<a href="javascript:void(0)"' .
            ' onclick="this.form.submit()">' .
            $this->text .
            '</a>';
    }

    public function __toString() {
        return $this->render();
    }
}
