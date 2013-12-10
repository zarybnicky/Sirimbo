<?php
namespace TKOlomouc\View\Helper;

use TKOlomouc\View\Partial;
use TKOlomouc\Utility\Miscellaneous;

class Zpravy extends Partial
{
    private $file = 'Helper/Zpravy';

    private $data  = array();
    private $count = 0;

    public function addItem($name, $date, $text)
    {
        $this->data[] = array(
            'name' => $name,
            'date' => $date,
            'text' => $text
        );
    }

    public function populate(array $data)
    {
        foreach ($data as $item) {
            list($date, $time) = explode(' ', $item['at_timestamp']);

            $this->addItem($item['at_jmeno'], Miscellaneous::formatDate($date), $item['at_text']);
        }

        return $this;
    }

    public function setCount($count)
    {
        $this->count = $count;

        return $this;
    }

    public function render()
    {
        if ($this->count <= 0 || count($this->data) <= 0) {
            return '';
        }
        if ($this->count > count($this->data)) {
            $this->count = count($this->data);
        }

        return $this->renderTemplate(
            $this->file,
            array(
                'data'  => array_slice($this->data, 0, $this->count),
                'count' => $this->count
            )
        );
    }

    public function __toString()
    {
        return $this->render();
    }
}
