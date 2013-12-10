<?php
namespace TKOlomouc\View\Helper;

use TKOlomouc\View\Partial;
use TKOlomouc\Type\DateFormat;
use TKOlomouc\Utility\Debug;
use TKOlomouc\Utility\Miscellaneous;

class Clanky extends Partial
{
    private $fileList      = 'Helper/ClankyList';
    private $fileSlideshow = 'Helper/ClankySlideshow';

    private $data        = null;
    private $offset      = 0;
    private $count       = 20;
    private $isSlideshow = false;

    public function offset($offset)
    {
        $this->offset = $offset;

        return $this;
    }

    public function count($count)
    {
        $this->count = $count;

        return $this;
    }

    public function setSlideshow($isOn)
    {
        $this->isSlideshow = $isOn;

        return $this;
    }

    public function addItem($id, $name, $date, $preview, $photoPath)
    {
        if ($date instanceof Date) {
            $date = $date->getDate(DateFormat::FORMAT_SIMPLIFIED);
        } else {
            $date = Miscellaneous::formatDate($date);
        }
        $this->data[] = array(
            'id'       => $id,
            'name'     => $name,
            'date'     => $date,
            'url'      => '/aktualne/' . $id,
            'preview'  => $preview,
            'photoUrl' => '/galerie/' . $photoPath
        );

        return $this;
    }

    public function populate(array $data)
    {
        foreach ($data as $item) {
            $this->addItem(
                $item['at_id'],
                $item['at_jmeno'],
                $item['at_timestamp_add'],
                $item['at_preview'],
                $item['at_foto_main']
            );
        }
    }

    public function render()
    {
        if ($this->count <= 0 || count($this->data) <= 0) {
            return '';
        }
        if (($this->count + $this->offset) > count($this->data)) {
            $this->count = count($this->data) - $this->offset;
        }

        if (!$this->isSlideshow) {
            return $this->renderTemplate(
                $this->fileList,
                array(
                    'offset' => $this->offset,
                    'count' => $this->count,
                    'data' => array_slice($this->data, $this->offset, $this->count)
                )
            );
        }
        if ($this->isSlideshow) {
            return $this->renderTemplate(
                $this->fileSlideshow,
                array(
                    'offset' => $this->offset,
                    'count' => $this->count,
                    'data' => array_slice($this->data, $this->offset, $this->count)
                )
            );
        }
    }

    public function __toString()
    {
        return $this->render();
    }
}
