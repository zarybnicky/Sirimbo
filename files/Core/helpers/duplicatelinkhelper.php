<?php
class DuplicateLinkHelper
{
    protected $link;
    protected $button;

    public function duplicateLink($link, $button = false)
    {
        $this->link = $link;
        $this->button = $button;
        return $this;
    }

    public function render()
    {
        if ($this->button) {
            return "<button style='padding:0' name='action' value='{$this->link}'>" .
                "<img alt='Duplikovat' src='/style/icon-files-o.png' />" .
                "</button>";
        }
        return "<a href='{$this->link}' title='Duplikocat'>" .
            "<img alt='Duplikovat' src='/style/icon-files-o.png' />" .
            "</a>";
    }

    public function __toString()
    {
        return $this->render();
    }
}
