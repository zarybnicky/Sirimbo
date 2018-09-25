<?php
class EditLinkHelper
{
    protected $link;
    protected $button;

    public function editLink($link, $button = false)
    {
        $this->link = $link;
        $this->button = $button;
        return $this;
    }

    public function render()
    {
        if ($this->button) {
            return "<button style='padding:0' name='action' value='{$this->link}'>" .
                "<img alt='Upravit' src='/style/icon-pencil.png' />" .
                "</button>";
        }
        return "<a href='{$this->link}' title='Upravit'>" .
            "<img alt='Upravit' src='/style/icon-pencil.png' />" .
            "</a>";
    }

    public function __toString()
    {
        return $this->render();
    }
}
