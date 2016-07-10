<?php
class RemoveLinkHelper
{
    protected $name;

    public function removeLink($name)
    {
        $this->name = $name;
        return $this;
    }

    public function render()
    {
        return (string) new Tag(
            'a',
            array('href' => $this->link, 'title' => 'Upravit'),
            new Tag(
                'img',
                array('alt' => 'Upravit', 'src' => '/style/icon-pencil.png')
            )
        );
    }

    public function __toString()
    {
        return $this->render();
    }
}
