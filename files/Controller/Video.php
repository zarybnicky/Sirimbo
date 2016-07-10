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
                        list($id, $query) = array_merge(explode('?', $item['v_uri']), array(''));
                        return array(
                            'name' => $item['v_name'],
                            'date' => $item['v_date'],
                            'uri' => "//www.youtube.com/embed/$id?$query&amp;autoplay=1",
                            'playlist' => (bool) $item['v_playlist'],
                            'thumbnail' => "https://i3.ytimg.com/vi/$id/mqdefault.jpg"
                        );
                    },
                    DBVideo::getAll()
                )
            )
        );
    }
}
