<?php
class NoticeHelper
{
    protected $type;
    protected $text;

    public function notice($text, $type = 'info')
    {
        $this->type = $type;
        $this->text = $text;
        return $this;
    }

    public function render()
    {
        if (!$this->text) {
            return '';
        }
        return '<div class="alert alert-' . $this->type . '" role="alert">'
            . $this->text . '</div>';
    }

    public function __toString()
    {
        return $this->render();
    }
}
