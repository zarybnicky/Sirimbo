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
            ' onclick="$(this).parent().parent().parent().submit()">' .
            $this->text .
            '</a>' .
            '<button type="submit">' .
            $this->text .
            '</button>';
    }

    public function __toString() {
        return $this->render();
    }
}
