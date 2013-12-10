<?php
namespace TKOlomouc\View\Helper;

use TKOlomouc\View\Partial;

class Menu extends Partial
{
    private $file = 'src/library/Template/Helper/Menu.tpl';

    private $float = 'right';
    private $data  = array();

    const FLOAT_NONE  = 'none';
    const FLOAT_LEFT  = 'left';
    const FLOAT_RIGHT = 'right';

    private $float_types = array(
        0 => self::FLOAT_NONE,
        1 => self::FLOAT_LEFT,
        2 => self::FLOAT_RIGHT
    );

    public function float($float)
    {
        if (!in_array($float, $this->float_types)) {
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

    public function render()
    {
        return $this->renderTemplate(
            $this->file,
            array(
                'float' => $this->float,
                'data'  => $this->data
            )
        );
    }

    public function __toString()
    {
        return $this->render();
    }
}
