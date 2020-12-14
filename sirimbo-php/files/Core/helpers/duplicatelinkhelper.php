<?php
class DuplicateLinkHelper
{
    protected $link;
    protected $button;

    public function __construct($link, $button = false)
    {
        $this->link = $link;
        $this->button = $button;
    }

    public function __toString()
    {
        if ($this->button) {
            return "<button style='padding:0' name='action' value='{$this->link}'>" .
                "<img alt='Duplikovat' src='/style/icon-files-o.png' />" .
                "</button>";
        }
        return "<a href='{$this->link}' title='Duplikovat'>" .
            "<img alt='Duplikovat' src='/style/icon-files-o.png' />" .
            "</a>";
    }
}
