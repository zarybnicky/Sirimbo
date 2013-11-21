<?php
namespace TKOlomouc\View\Helper;

use TKOlomouc\View\Partial;
use TKOlomouc\Type\DateFormat;

class Clanky extends Partial
{
    private $fileList      = 'src/library/Template/Helper/ClankyList.tpl';
    private $fileSlideshow = 'src/library/Template/Helper/ClankySlideshow.tpl';

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

    public function populateFromDb()
    {
        //TODO: Populate from DB
        /*
        if ($this->data === null) {
            $input = DBAktuality::getAktuality(AKTUALITY_CLANKY);
            foreach ($input as $item) {
                $this->addData($item);
            }
        }
        */
    }

    public function render()
    {
        if (($this->count + $this->offset) > count($this->data)) {
            $this->count = count($this->data) - $this->offset;
        }

        if (!$this->isSlideshow) {
            return $this->renderTemplate(
                $this->fileList,
                array(
                    'offset' => $this->offset,
                    'count'  => $this->count,
                    'data'   => $this->data
                )
            );
        }

        if ($this->isSlideshow && $this->count > 0) {
            return $this->renderTemplate(
                $this->fileSlideshow,
                array(
                    'offset' => $this->offset,
                    'count'  => $this->count,
                    'data'   => $this->data
                )
            );
        }
        return '';
    }

    public function __toString()
    {
        return $this->render();
    }
}