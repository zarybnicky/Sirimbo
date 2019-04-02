<?php
class Controller_Video extends Controller_Abstract
{
    public function view($request)
    {
        $data = array_map(
            function ($item) {
                list($id, $query) = array_merge(explode('?', $item['v_uri']), ['']);
                return [
                    'title' => $item['v_title'],
                    'date' => $item['v_created_at'],
                    'uri' => "//www.youtube.com/embed/$id?$query&amp;autoplay=1",
                    'playlist' => $item['v_playlist'],
                    'thumbnail' => "https://i3.ytimg.com/vi/$id/mqdefault.jpg"
                ];
            },
            DBVideo::getAll()
        );
        $this->render('files/View/Main/Video.inc', [
            'header' => 'Video',
            'data' => $data
        ]);
    }
}
