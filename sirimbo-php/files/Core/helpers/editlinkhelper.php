<?php
class EditLinkHelper
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
            return "<button class='a' name='action' value='{$this->link}'>" .
                "<img alt='Upravit' src='/style/icon-pencil.png' />" .
                "</button>";
        }
        return "<a href='{$this->link}' title='Upravit'>" .
            "<img alt='Upravit' src='/style/icon-pencil.png' />" .
            "</a>";
    }
}
