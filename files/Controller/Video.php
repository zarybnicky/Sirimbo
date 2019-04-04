<?php
class Controller_Video extends Controller_Abstract
{
    public function view($request)
    {
        if ($request->get('playlist') == 'other') {
            $videos = DBVideo::getOrphan();
            $playlist = 'Nezařazená videa';
        } elseif ($request->get('playlist')) {
            $videos = DBVideo::getByPlaylist($request->get('playlist'));
            $playlist = DBVideoList::getSingle($request->get('playlist'))['vl_title'];
        } else {
            $videos = [];
            $playlist = null;
        }
        $videos = array_map(
            function ($item) {
                list($id, $query) = array_merge(explode('?', $item['v_uri']), ['']);
                return [
                    'title' => $item['v_title'],
                    'date' => $item['v_created_at'],
                    'uri' => "//www.youtube.com/embed/$id?$query&amp;autoplay=1",
                    'thumbnail' => "https://i3.ytimg.com/vi/$id/mqdefault.jpg"
                ];
            },
            $videos
        );
        $playlists = array_map(
            function ($item) {
                return ['id' => $item['vl_id'], 'title' => $item['vl_title']];
            },
            DBVideoList::getAll()
        );
        $playlists[] = ['id' => 'other', 'title' => 'Nezařazená videa'];
        $this->render('files/View/Main/Video.inc', [
            'header' => 'Video',
            'subheader' => $playlist,
            'videos' => $videos,
            'playlist' => $request->get('playlist'),
            'playlists' => $playlists
        ]);
    }
}
