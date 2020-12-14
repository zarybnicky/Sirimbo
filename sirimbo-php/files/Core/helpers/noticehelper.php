<?php
class NoticeHelper
{
    protected $type;
    protected $text;

    public function __construct($text, $type = 'info')
    {
        $this->type = $type;
        $this->text = $text;
        return $this;
    }

    public function __toString()
    {
        if (!$this->text) {
            return '';
        }
        return '<div class="alert alert-' . $this->type . '" role="alert">'
            . $this->text . '</div>';
    }
}
