<?php
class RemoveLinkHelper
{
    protected $link;

    public function removeLink($link)
    {
        $this->link = $link;
        return $this;
    }

    public function render()
    {
        return (string) new Tag(
            'a',
            ['href' => $this->link, 'title' => 'Upravit'],
            new Tag('img', ['alt' => 'Upravit', 'src' => '/style/icon-trash-o.png'])
        );
    }

    public function __toString()
    {
        return $this->render();
    }
}
