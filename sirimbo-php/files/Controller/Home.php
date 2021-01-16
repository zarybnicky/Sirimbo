<?php
namespace Olymp\Controller;

class Home
{
    public static function get()
    {
        $articles = \DBAktuality::getAktuality(1);

        $highlights = array_for(array_slice($articles, 0, 3), fn($val) => [
            'uri'  => '/aktualne/' . $val['at_id'],
            'name' => $val['at_jmeno'],
            'description' => $val['at_preview'],
            'title_photo_uri' => $val['at_foto_main'] ? '/galerie/' . $val['gf_path'] : ''
        ]);

        $moreArticles = array_for(array_slice($articles, 3, 2), fn($val) => [
            'uri'  => '/aktualne/' . $val['at_id'],
            'name' => $val['at_jmeno'],
            'description' => $val['at_preview'],
            'title_photo_uri' => $val['at_foto_main'] ? '/galerie/' . $val['gf_path'] : ''
        ]);

        $videos = array_map(
            function ($id) {
                $x = \DBVideo::getSingle($id);
                list($id, $query) = array_merge(explode('?', $x['v_uri']), ['']);
                return [
                    'title' => $x['v_title'],
                    'link' => "https://www.youtube.com/watch?v=$id" . ($query ? "&amp;$query" : ''),
                    'image' => "https://i3.ytimg.com/vi/$id/hqdefault.jpg"
                ];
            },
            array_filter([
                \DBParameters::get('title_video1'),
                \DBParameters::get('title_video2'),
                \DBParameters::get('title_video3')
            ])
        );

        \Render::twig('Main/Home.twig', [
            'highlights' => $highlights,
            'moreArticles' => $moreArticles,
            'videos' => $videos
        ]);
    }
}
