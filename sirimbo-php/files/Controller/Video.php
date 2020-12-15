<?php
namespace Olymp\Controller;

class Video
{
    public static function get()
    {
        $playlist = $_GET['playlist'] ?? null;
        if ($playlist == 'other') {
            $videos = \DBVideo::getOrphan();
            $playlist = 'Nezařazená videa';
        } elseif ($playlist) {
            $videos = \DBVideo::getByPlaylist($playlist);
            $playlist = \DBVideoList::getSingle($playlist)['vl_title'];
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
            fn($item) => ['id' => $item['vl_id'], 'title' => $item['vl_title']],
            \DBVideoList::getAll()
        );
        $playlists[] = ['id' => 'other', 'title' => 'Nezařazená videa'];
        new \RenderHelper('files/View/Main/Video.inc', [
            'header' => 'Video',
            'subheader' => $playlist,
            'videos' => $videos,
            'playlist' => $playlist,
            'playlists' => $playlists
        ]);
    }
}
