<?php
class Sidebar
{
    protected $data;
    protected $request;
    protected $toplevel;

    public function __construct($data, $uri)
    {
        $this->data = $data;
        $this->uri = $uri;
    }

    public function __toString()
    {
        $list = new Tag('ul');

        foreach ($this->data as $item) {
            $link = new Tag(
                'a',
                array(
                    'class' => (
                        (strripos($this->uri, trim($item[1], '/')) === 0)
                        ? 'emph no-a' : ''
                    ),
                    'href' => $item[1] ?: ''
                ),
                $item[0]
            );
            $list->add(
                new Tag('li', array('class' => $item[2] ? 'more' : ''), $link)
            );
        }

        $root = new Tag('div', array('class' => 'container full menu-side'));
        $root->add($list);
        return (string) $root;
    }
}
