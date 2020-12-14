<?php
class RemoveLinkHelper
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
                "<img alt='Odstranit' src='/style/icon-trash-o.png' />" .
                "</button>";
        }
        return "<a href='{$this->link}' title='Odstranit'>" .
            "<img alt='Odstranit' src='/style/icon-trash-o.png' />" .
            "</a>";
    }
}
