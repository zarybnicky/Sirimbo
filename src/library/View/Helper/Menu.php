<?php
namespace TKOlomouc\View\Helper;

use TKOlomouc\View\Partial;

class Menu extends Partial
{
    private final $file = 'Helper/Menu.tpl';

    private $float = 'right';
    private $data  = array();

    const FLOAT = array(
        0 => 'none',
        1 => 'left',
        2 => 'right'
    );
    const FLOAT_NONE  = 'none';
    const FLOAT_LEFT  = 'left';
    const FLOAT_RIGHT = 'right';

    public function float($float)
    {
        if(!in_array($float, self::FLOAT)) {
            $float = self::FLOAT_NONE;
        }

        $this->float = $float;

        return $this;
    }

    public function item($name, $url, $button = false, $overwrite = false)
    {
        if ($overwrite) {
            $this->data = array();
        }

        $this->data[] = array(
            'name'   => $name,
            'url'    => $url,
            'button' => $button
        );

        return $this;
    }

    function render()
    {
        return $this->renderTemplate(
            $this->file,
            array(
        	    'float' => $this->float,
                'data'  => $this->data
            )
        );
    }

    function __toString()
    {
        return $this->render();
    }
}