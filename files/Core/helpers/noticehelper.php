<?php
class NoticeHelper
{
    protected $text;

    public function notice($text)
    {
        $this->text = $text;
        return $this;
    }

    public function render()
    {
        if (!$this->text) {
            return '';
        }
        return (string) new Tag('div', ['class' => 'notice'], $this->text);
    }

    public function __toString()
    {
        return $this->render();
    }
}
