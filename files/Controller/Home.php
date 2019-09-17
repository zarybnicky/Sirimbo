<?php
class Controller_Home extends Controller_Abstract
{
    public function view($request)
    {
        $uri = $request->getURI();
        if (in_array(strtolower($uri), ['/', '/home', '/index.php'])) {
            return $this->home();
        }

        if (!($page = DBPage::getByUrl($uri))) {
            throw new NotFoundException("Page '$uri' not found");
        }

        if ($page['p_type'] === 'plain' || $page['p_type'] === 'quill') {
            $this->render('files/View/Plain.inc', array_merge(
                [
                    'header' => $page['p_title'],
                    'html' => $page['p_html']
                ],
                json_decode($page['p_parameters'])
            ));
        } else {
            //Case-by-case rendering?
            //K-V map of special render Class::methods?
            //Just accept Class::method from the use?rl
            throw new Exception('TODO');
        }
    }

    public function home()
    {
        $articles = DBAktuality::getAktuality(AKTUALITY_CLANKY);

        $highlights = array_map(
            function ($val) {
                return [
                    'uri'  => '/aktualne/' . $val['at_id'],
                    'name' => $val['at_jmeno'],
                    'date' => formatDate($val['at_timestamp'], true),
                    'description' => $val['at_preview'],
                    'title_photo_uri' => (
                        $val['at_foto_main']
                        ? '/galerie/' . $val['gf_path']
                        : ''
                    )
                ];
            },
            array_slice($articles, 0, 3)
        );

        $moreArticles = array_map(
            function ($val) {
                return [
                    'uri'  => '/aktualne/' . $val['at_id'],
                    'name' => $val['at_jmeno'],
                    'date' => formatDate($val['at_timestamp'], true),
                    'description' => $val['at_preview'],
                    'title_photo_uri' => (
                        $val['at_foto_main']
                        ? '/galerie/' . $val['gf_path']
                        : ''
                    )
                ];
            },
            array_slice($articles, 3, 2)
        );

        $videos = array_map(
            function ($id) {
                $x = DBVideo::getSingle($id);
                list($id, $query) = array_merge(explode('?', $x['v_uri']), ['']);
                return [
                    'title' => $x['v_title'],
                    'link' => "https://www.youtube.com/watch?v=$id" . ($query ? "&amp;$query" : ''),
                    'image' => "https://i3.ytimg.com/vi/$id/hqdefault.jpg"
                ];
            },
            array_filter([
                DBParameters::get('title_video1'),
                DBParameters::get('title_video2'),
                DBParameters::get('title_video3')
            ])
        );

        $this->render('files/View/Main/Home.inc', [
            'highlights' => $highlights,
            'moreArticles' => $moreArticles,
            'videos' => $videos
        ]);
    }
}
