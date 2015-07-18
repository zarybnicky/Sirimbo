<?php
class Controller_Video extends Controller_Abstract
{
    public function view($request)
    {
        $this->render(
            'files/View/Main/Video.inc',
            array(
                'data' => array_map(
                    function ($item) {
                        return array(
                            'name' => $item['v_name'],
                            'uri' => $item['v_uri'],
                            'playlist' => (bool) $item['v_playlist']
                        );
                    },
                    DBVideo::getAll()
                )
            )
        );
    }
}