<?php
namespace TKOlomouc\View\Helper;

use TKOlomouc\View\Partial;

class Table extends Partial
{
    private final $file = 'Helper/Table.tpl';

    private $style      = '';
    private $data       = array();
    private $columns    = array();
    private $showHeader = true;

    public function style($style)
    {
        $this->style = ' style="' . $style . '"';

        return $this;
    }

    public function data($d)
    {
        if ($d instanceof \Traversable || (is_array($d) && is_array(reset($d)))) {
            $this->data = $d;
        }
        return $this;
    }

    public function column($id, $name, $class = null, $style = null, $overwrite = false)
    {
        if($overwrite == true) {
            $this->columns = array();
        }
        $style = ($class !== null ? (' class="' . $class . '"') : '')
                . ($style !== null ? (' style="' . $style . '"') : '');

        $this->columns[] = array(
            'dataKey' => $id,
            'name'    => $name,
            'style'   => $style
        );
        return $this;
    }

    public function showHeader($bool)
    {
        $this->showHeader = (bool) $bool;

        return $this;
    }

    public function __toString()
    {
        return $this->render();
    }

    public function render()
    {
        if ($this->data === null && $this->columns === null) {
            return '';
        }

        return $this->renderTemplate(
            $this->file,
            array(
        	    'style'      => $this->style,
                'showHeader' => $this->showHeader,
                'data'       => $this->data,
                'columns'    => $this->columns
            )
        );
    }
}
?>