<?php
class ResultsHelper
{
    protected $data;

    public function results($data)
    {
        $this->data = $data;
        return $this;
    }

    public function render()
    {
        $data = array_map(
            function ($val) {
                return array(
                    'text' => $val['at_text']
                );
            },
            $this->data
        );

        $r = new Renderer();
        return $r->render(
            'files/View/Helper/Zpravy.inc',
            array('data' => $data)
        );
    }

    public function __toString()
    {
        return $this->render();
    }
}