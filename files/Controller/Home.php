<?php
class Controller_Home extends Controller_Abstract
{
    public function view($request)
    {
        if (!$request->getURI()) {
            if (NABOR) {
                $this->redirect('/nabor');
            }
            $this->redirect('/home');
        }

        $articles = DBAktuality::getAktuality(AKTUALITY_CLANKY);
        $results = DBAktuality::getAktuality(AKTUALITY_KRATKE);

        $highlights = array_slice($articles, 0, 3);
        $moreArticles = array_slice($articles, 3, 9);

        $videos = array_filter(array(
            ($id = DBParameters::get('title_video1')) ? DBVideo::getSingle($id) : null,
            ($id = DBParameters::get('title_video2')) ? DBVideo::getSingle($id) : null,
            ($id = DBParameters::get('title_video3')) ? DBVideo::getSingle($id) : null,
            ($id = DBParameters::get('title_video4')) ? DBVideo::getSingle($id) : null
        ));

        $videos = new Tag(
            'ul',
            array('class' => 'videos'),
            array_map('renderVideo', $videos),
            new Tag('div', array('class' => 'clearfix'))
        );

        $this->render(
            'files/View/Main/Home.inc',
            array(
                'highlights' => $highlights,
                'moreArticles' => $moreArticles,
                //'results' => $results,
                'videos' => $videos
            )
        );
    }
}

function renderVideo($x)
{
    list($id, $query) = array_merge(explode('?', $x['v_uri']), array(''));
    return '<li style="float:left;width:49%;padding-right:1%"><a target="_blank" class="no-a" href="'
         . "https://www.youtube.com/watch?v=$id" . ($query ? "&amp;$query" : '')
         . '"><img alt="" src="'
         . "https://i3.ytimg.com/vi/$id/hqdefault.jpg"
         . '" /></a></li>';
}
