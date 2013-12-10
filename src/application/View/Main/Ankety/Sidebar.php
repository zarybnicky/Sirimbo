<?php
namespace TKOlomouc\View\Main\Ankety;

use TKOlomouc\Model\Database\Adapter;
use TKOlomouc\Model\DBAnkety;
use TKOlomouc\Utility\Miscellaneous;
use TKOlomouc\View\Partial;

class Sidebar extends Partial
{
    private $file = 'Main/Ankety/Sidebar';

    public function render()
    {
        $data = DBAnkety::getAvailableAnketyWithItems(Miscellaneous::getIP());

        if (!$data) {
            return '';
        }

        $data = $data[array_rand($data)];
        $items = array();

        foreach ($data['items'] as $item) {
            $items[] = array(
                'id' => $item['aki_id'],
                'text' => $item['aki_text']
            );
        }

        return $this->renderTemplate(
            $this->file,
            array(
        	    'id' => $data['ak_id'],
                'text' => $data['ak_text'],
                'items' => $items
            )
        );
    }
}
