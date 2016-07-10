<?php
class MenuHelper
{
    protected $content;

    public function menu()
    {
        $this->content = array();
        return $this;
    }

    public function content($name, $url, $button = false, $replace = false)
    {
        if ($replace) {
            $this->content = array();
        }
        $this->content[] = array($name, $url, $button);

        return $this;
    }

    public function render()
    {
        if (!$this->content) {
            return '';
        }
        return (string) new Tag(
            'div',
            array('class' => 'sticky', 'style' => 'width:150px;float:right'),
            new Tag(
                'div',
                array('style' => 'z-index:100;width:inherit;border:'
                      . '1px solid #aaa;background:#ddd;margin-top:2px;padding:3px 1px;'),
                array_map(
                    function ($data) {
                        list($name, $url, $button) = $data;
                        return new Tag(
                            'div',
                            array(),
                            $button
                            ? new Tag(
                                'button',
                                array(
                                    'style' => 'padding:0',
                                    'name' => 'action',
                                    'value' => $url
                                ),
                                $name
                            )
                            : new Tag(
                                'a',
                                array('style' => 'padding:0 3px', 'href' => $url),
                                $name
                            )
                        );
                    },
                    $this->content
                )
            )
        );
    }
    public function __toString()
    {
        return $this->render();
    }
}
