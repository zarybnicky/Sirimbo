<?php
namespace TKOlomouc\View\Helper;

use TKOlomouc\View\Partial;

class Zpravy extends Partial
{
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

    public function setData(array $data, $overwrite = true)
    {
        if ($overwrite) {
            $this->data = array();
        }

        foreach ($data as $item) {
            list($date, $time) = explode(' ', $this->data[$i]['at_timestamp']);

            $this->addItem($item['at_jmeno'], formatDate($date), $item['at_text']);
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
        if ($this->count > count($this->data)) {
            $this->count = count($this->data);
        }

        return $this->renderTemplate(
            $this->file,
            array(
        	    'data'  => $this->data,
                'count' => $this->count
            )
        );
    }

    public function __toString()
    {
        return $this->render();
    }
}