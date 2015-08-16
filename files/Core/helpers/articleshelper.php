<?php
class ArticlesHelper
{
    protected $data;

    public function articles($data)
    {
        $this->data = $data;
        return $this;
    }

    public function render()
    {
        $data = array_map(
            function ($val) {
                return array(
                    'uri'  => '/aktualne/' . $val['at_id'],
                    'name' => $val['at_jmeno'],
                    'date' => formatDate($val['at_timestamp'], true),
                    'description' => $val['at_preview'],
                    'title_photo_uri' => (
                        $val['at_foto_main']
                        ? '/galerie/' . $val['at_foto_main']
                        : ''
                    ),
                    'category' => 'Zprávy'
                    //FIXME: Články - kategorie (tagy?)
                );
            },
            $this->data
        );

        $r = new Renderer();
        return $r->render(
            'files/View/Helper/ViceClanku.inc',
            array('data' => $data)
        );
    }

    public function __toString()
    {
        return $this->render();
    }
}
