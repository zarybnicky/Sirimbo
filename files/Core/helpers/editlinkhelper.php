<?php
class EditLinkHelper
{
    protected $link;

    public function editLink($link)
    {
        $this->link = $link;
        return $this;
    }

    public function render()
    {
        return (string) new Tag(
            'a',
            ['href' => $this->link, 'title' => 'Upravit'],
            new Tag('img', ['alt' => 'Upravit', 'src' => '/style/icon-pencil.png'])
        );
    }

    public function __toString()
    {
        return $this->render();
    }
}
